// Macro template expansion - runtime expansion using matched bindings
//
// This module handles template expansion for R7RS syntax-rules macros at expansion time.
// It takes compiled templates and match contexts and generates the expanded output.
//
// **R7RS COMPLIANCE STATUS:**
//
// **CRITICAL GAP - NO NESTED ELLIPSIS SUPPORT:**
// - This expander CANNOT handle nested ellipsis templates (e.g., `(x ... ...)`).
// - It receives a correct, hierarchical `Binding` structure from the matcher but lacks the
//   logic to process more than one level of `Binding::Repeated`.
// - When it encounters a template with multiple `...`, it does not know how to "unzip"
//   the multiple levels of repetition from the bindings.
// - This is the primary source of non-conformance for nested ellipsis and a major R7RS deviation.
//
// **PARTIALLY IMPLEMENTED R7RS FEATURES:**
// - ✓ Variable substitution: Replace pattern variables with their bound values.
// - ✓ Ellipsis replication: `(template ...)` replicates based on a single level of repetition.
// - ✗ Nested ellipsis expansion: `((template ...) ...)` or `(x ... ...)` is NOT supported and will fail.
//
// **R7RS RESTRICTED** (Inherited from compiler/matcher):
// - ✗ Vector templates: `#(template ...)` - project has no vector support.
// - ✗ Improper list templates: `(a b . rest)` - project only supports proper lists.
//
// **IMPLEMENTATION NOTES:**
// - The function `expand_ellipsis_template` is the source of the failure. It is only capable
//   of handling a single layer of `Binding::Repeated` and does not recursively descend
//   into nested `Repeated` structures.
// - Fixing this would require a significant redesign of the expansion logic to handle
//   multiple, independent repetition counts from nested ellipsis contexts.

use crate::macro_compiler::{CompiledTemplate, MacroError, MacroTemplate};
use crate::macro_matcher::{Binding, MatchContext};
use crate::value::Value;

// Global debug flag - set via environment variable MACRO_DEBUG=1
static DEBUG: std::sync::LazyLock<bool> =
    std::sync::LazyLock::new(|| std::env::var("MACRO_DEBUG").is_ok());

macro_rules! debug_trace {
    ($($arg:tt)*) => {
        if *DEBUG {
            eprintln!("[EXPANDER DEBUG] {}", format!($($arg)*));
        }
    };
}

/// Expand a compiled template using the bindings from pattern matching
///
/// This is the main entry point for template expansion.
/// pattern_variables should be the set of actual pattern variables (from the compiled pattern),
/// NOT the template variables (which include literals).
pub fn expand_template(
    compiled: &CompiledTemplate,
    context: &MatchContext,
    pattern_variables: &std::collections::HashSet<String>,
) -> Result<Value, MacroError> {
    debug_trace!("=== EXPANDING TEMPLATE ===");
    debug_trace!("Template variables: {:?}", compiled.variables);
    debug_trace!("Pattern variables: {:?}", pattern_variables);
    debug_trace!("Max depth: {}", compiled.max_ellipsis_depth);

    expand_template_internal(
        &compiled.template,
        context,
        0,
        false, // Not in a quote context initially
        pattern_variables,
    )
}

/// Internal template expansion with depth tracking
///
/// current_depth tracks how many ellipsis layers we're inside (starts at 0)
/// in_quote_context tracks whether we are inside a (quote ...) form
/// pattern_variables contains the set of actual pattern variables (to distinguish from literals)
fn expand_template_internal(
    template: &MacroTemplate,
    context: &MatchContext,
    current_depth: usize,
    in_quote_context: bool,
    pattern_variables: &std::collections::HashSet<String>,
) -> Result<Value, MacroError> {
    debug_trace!(
        "expand_template_internal: {:?} at depth {} (in_quote: {})",
        template,
        current_depth,
        in_quote_context
    );

    match template {
        // Literal values pass through unchanged
        MacroTemplate::Literal(value) => {
            debug_trace!("  -> literal: {}", value_compact(value));
            Ok(value.clone())
        }

        // Variable substitution - look up binding and extract value at current depth
        // If in a quote context, or if variable is not a pattern variable, treat as literal symbol
        MacroTemplate::Variable(name) => {
            if in_quote_context || !pattern_variables.contains(name) {
                // In quote or not a pattern var -> literal symbol
                debug_trace!(
                    "  -> literal symbol '{}' (in_quote or not a pattern var)",
                    name
                );
                return Ok(Value::Symbol(name.clone()));
            }

            match context.get(name) {
                Some(binding) => {
                    debug_trace!(
                        "  -> variable {} with binding: {}",
                        name,
                        binding.debug_format()
                    );
                    // Extract value at current depth
                    extract_value_at_depth(binding, current_depth, name)
                }
                None => {
                    // Pattern variable not found in bindings - this indicates a matcher bug
                    Err(MacroError(format!(
                        "Internal error: pattern variable '{}' not found in match context",
                        name
                    )))
                }
            }
        }

        // List template - expand each element and collect results
        MacroTemplate::List(templates) => {
            debug_trace!("  -> list template with {} elements", templates.len());

            // Check for (quote ...) or (quasiquote ...) forms
            // R7RS: In templates, quote is just a symbol - the pattern variables inside still get substituted
            // The quote context is ONLY for preventing underscore errors during compilation,
            // NOT for preventing variable substitution during expansion.
            if !templates.is_empty() {
                if let MacroTemplate::Variable(quote_symbol) = &templates[0] {
                    if quote_symbol == "quote" || quote_symbol == "quasiquote" {
                        // Validate the quote form structure
                        if templates.len() != 2 {
                            return Err(MacroError(format!(
                                "Malformed {} form in template: expected ({} <datum>), found list with {} elements",
                                quote_symbol,
                                quote_symbol,
                                templates.len()
                            )));
                        }

                        // Expand the quoted datum NORMALLY - pattern variables get substituted
                        // The in_quote_context is passed through unchanged
                        let expanded_datum = expand_template_internal(
                            &templates[1],
                            context,
                            current_depth,
                            in_quote_context, // NOT true - quote doesn't stop substitution!
                            pattern_variables,
                        )?;

                        return Ok(Value::List(vec![
                            Value::Symbol(quote_symbol.clone()),
                            expanded_datum,
                        ]));
                    }
                }
            }

            // Not a quote form, expand normally
            expand_list_template(
                templates,
                context,
                current_depth,
                in_quote_context,
                pattern_variables,
            )
        }

        // Ellipsis template - replicate sub-template based on binding repetition count
        MacroTemplate::Ellipsis(sub_template) => {
            if in_quote_context {
                // R7RS: Ellipsis inside a quote is a literal `...`
                return Ok(Value::Symbol("...".to_string()));
            }
            debug_trace!("  -> ellipsis template");
            expand_ellipsis_template(
                sub_template,
                context,
                current_depth,
                in_quote_context,
                pattern_variables,
            )
        }
    }
}

/// Expand a list of templates into a list value
fn expand_list_template(
    templates: &[MacroTemplate],
    context: &MatchContext,
    current_depth: usize,
    in_quote_context: bool,
    pattern_variables: &std::collections::HashSet<String>,
) -> Result<Value, MacroError> {
    let mut result = Vec::new();

    for (i, template) in templates.iter().enumerate() {
        // Check if this is an ellipsis template
        if let MacroTemplate::Ellipsis(sub_template) = template {
            debug_trace!("  -> ellipsis at index {}", i);

            if in_quote_context {
                // Ellipsis inside a quote is a literal `...`
                result.push(Value::Symbol("...".to_string()));
                continue;
            }

            // Ellipsis must be the last element (enforced by compiler)
            // Expand it and append all results
            let expanded = expand_ellipsis_template(
                sub_template,
                context,
                current_depth,
                in_quote_context,
                pattern_variables,
            )?;
            if let Value::List(items) = expanded {
                result.extend(items);
            } else {
                return Err(MacroError(
                    "Ellipsis expansion must produce a list".to_string(),
                ));
            }
        } else {
            // Regular template - expand and append single result
            let expanded = expand_template_internal(
                template,
                context,
                current_depth,
                in_quote_context,
                pattern_variables,
            )?;
            result.push(expanded);
        }
    }

    debug_trace!(
        "  -> expanded list: {}",
        value_compact(&Value::List(result.clone()))
    );
    Ok(Value::List(result))
}

/// Expand an ellipsis template by replicating the sub-template
///
/// The repetition count is determined by the Repeated bindings of variables in the sub-template.
/// All variables in the sub-template must have the same repetition count (validated by compiler).
fn expand_ellipsis_template(
    sub_template: &MacroTemplate,
    context: &MatchContext,
    current_depth: usize,
    in_quote_context: bool,
    pattern_variables: &std::collections::HashSet<String>,
) -> Result<Value, MacroError> {
    debug_trace!("expand_ellipsis_template at depth {}", current_depth + 1);

    // Find all variables in the sub-template, but only include actual pattern variables
    let all_vars = collect_template_variables(sub_template);
    let vars: Vec<String> = all_vars
        .into_iter()
        .filter(|v| pattern_variables.contains(v))
        .collect();
    debug_trace!("  Variables in ellipsis: {:?}", vars);

    // Determine repetition count from the first variable's binding
    let repetition_count = if vars.is_empty() {
        // No variables - ellipsis with only literals (like (_ ...))
        // This is unusual but valid - need to determine count from context
        // For now, return empty list (zero repetitions)
        debug_trace!("  No variables in ellipsis - producing empty list");
        return Ok(Value::List(Vec::new()));
    } else {
        // Get the first variable's binding to determine count
        let first_var = &vars[0];
        let binding = context.get(first_var).ok_or_else(|| {
            MacroError(format!(
                "Ellipsis variable '{}' not found in bindings",
                first_var
            ))
        })?;

        // The binding should be a Repeated at the current depth
        match binding {
            Binding::Repeated { count, .. } => {
                debug_trace!("  Repetition count: {}", count);
                *count
            }
            Binding::Direct(_) => {
                return Err(MacroError(format!(
                    "Variable '{}' has Direct binding but expected Repeated for ellipsis",
                    first_var
                )));
            }
        }
    };

    // Handle empty ellipsis case
    if repetition_count == 0 {
        debug_trace!("  Empty ellipsis - producing empty list");
        return Ok(Value::List(Vec::new()));
    }

    // Replicate the sub-template for each repetition
    let mut result = Vec::new();
    for i in 0..repetition_count {
        debug_trace!("  Replication {}/{}", i + 1, repetition_count);

        // Create a temporary context for this iteration
        // Extract the i-th element from each Repeated binding
        let mut temp_context = MatchContext::new();

        for var in &vars {
            let binding = context
                .get(var)
                .ok_or_else(|| MacroError(format!("Ellipsis variable '{}' not found", var)))?;

            match binding {
                Binding::Repeated { elements, .. } => {
                    if i >= elements.len() {
                        return Err(MacroError(format!(
                            "Index {} out of bounds for variable '{}' with {} elements",
                            i,
                            var,
                            elements.len()
                        )));
                    }
                    temp_context.bind(var.clone(), elements[i].clone());
                }
                Binding::Direct(_) => {
                    return Err(MacroError(format!(
                        "Variable '{}' has Direct binding in ellipsis context",
                        var
                    )));
                }
            }
        }

        // Expand the sub-template with this iteration's bindings
        // Note: temp_context has Direct bindings (depth 0), so we expand at depth 0
        let expanded = expand_template_internal(
            sub_template,
            &temp_context,
            0,
            in_quote_context,
            pattern_variables,
        )?;
        result.push(expanded);
    }

    debug_trace!("  Ellipsis produced {} items", result.len());
    Ok(Value::List(result))
}

/// Extract a value from a binding at a specific depth
///
/// - At depth 0: Binding must be Direct, return the value
/// - At depth N > 0: Not applicable - ellipsis expansion handles depth > 0
fn extract_value_at_depth(
    binding: &Binding,
    depth: usize,
    var_name: &str,
) -> Result<Value, MacroError> {
    if depth == 0 {
        // At depth 0, we expect a Direct binding
        match binding {
            Binding::Direct(value) => Ok(value.clone()),
            Binding::Repeated { .. } => Err(MacroError(format!(
                "Internal error: variable '{}' has Repeated binding at depth 0, expected Direct",
                var_name
            ))),
        }
    } else {
        // At depth > 0, ellipsis expansion should have handled this
        Err(MacroError(format!(
            "Internal error: attempting to extract variable '{}' at depth {} without ellipsis expansion",
            var_name, depth
        )))
    }
}

/// Collect all variable names from a template
fn collect_template_variables(template: &MacroTemplate) -> Vec<String> {
    let mut vars = Vec::new();
    collect_template_variables_impl(template, &mut vars);
    vars
}

fn collect_template_variables_impl(template: &MacroTemplate, vars: &mut Vec<String>) {
    match template {
        MacroTemplate::Variable(name) => {
            if !vars.contains(name) {
                vars.push(name.clone());
            }
        }
        MacroTemplate::List(templates) => {
            for t in templates {
                collect_template_variables_impl(t, vars);
            }
        }
        MacroTemplate::Ellipsis(sub_template) => {
            collect_template_variables_impl(sub_template, vars);
        }
        MacroTemplate::Literal(_) => {}
    }
}

// Helper for compact value display in debug output
fn value_compact(v: &Value) -> String {
    match v {
        Value::Symbol(s) => s.clone(),
        Value::Integer(n) => n.to_string(),
        Value::Boolean(b) => {
            if *b {
                "#t".to_string()
            } else {
                "#f".to_string()
            }
        }
        Value::String(s) => format!("\"{}\"", s),
        Value::List(items) => {
            let item_strs: Vec<_> = items.iter().map(value_compact).collect();
            format!("({})", item_strs.join(" "))
        }
        _ => format!("{:?}", v),
    }
}
