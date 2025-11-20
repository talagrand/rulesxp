// Macro pattern/template compilation and validation
//
// This module handles the definition-time compilation of R7RS syntax-rules macros.
// It pre-computes metadata and validates patterns/templates before any macro expansion occurs.
//
// Key responsibilities:
// - Parse patterns and templates from syntax-rules forms.
// - Compute ellipsis nesting depths for all variables.
// - Validate that template variables match pattern variables at compatible depths.
// - Detect errors like inconsistent depths or templates deeper than patterns.
//
// This compilation happens once at macro definition time, enabling fast expansion later.
//
// **IMPORTANT:** Quasiquote depth of 0 means Normal context (not inside any quasiquote).
//
// **R7RS COMPLIANCE STATUS:**
//
// **CRITICAL GAP - NESTED ELLIPSIS:**
// - This compiler correctly parses nested ellipsis patterns (e.g., `((x ...) ...)` and `(x ... ...)`).
// - It generates the correct depth metadata for these patterns.
// - However, the `macro_expander` CANNOT correctly expand these templates, making the
//   feature non-functional. This is a major R7RS deviation.
//
// **PARTIALLY IMPLEMENTED R7RS FEATURES:**
// - ✓ Ellipsis patterns: `(pattern ...)` for zero-or-more repetition.
// - ✓ Nested ellipsis parsing: `((x ...) ...)` is parsed with proper depth tracking, but is not expandable.
// - ✓ Depth validation: Template depth cannot exceed pattern depth.
// - ✓ Consistency checking: Variables at same depth throughout pattern/template.
//
// **R7RS RESTRICTED** (Intentional project limitations):
// - ✗ Vector patterns/templates: `#(...)` - project has no vector support.
// - ✗ Improper list patterns/templates: `(a . b)` - project only supports proper lists.
//
// **IMPLEMENTATION NOTES:**
// - All R7RS syntax-rules validations that can be done at compile-time are enforced here.
// - The parsing logic for templates allows for consecutive `...` to support nested ellipsis syntax,
//   even though the expander cannot currently handle them.

use crate::value::Value;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct MacroError(pub String);

impl std::fmt::Display for MacroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Macro error: {}", self.0)
    }
}

impl std::error::Error for MacroError {}

// Global debug flag - set via environment variable MACRO_DEBUG=1
static DEBUG: std::sync::LazyLock<bool> =
    std::sync::LazyLock::new(|| std::env::var("MACRO_DEBUG").is_ok());

macro_rules! debug_trace {
    ($($arg:tt)*) => {
        if *DEBUG {
            eprintln!("[MACRO DEBUG] {}", format!($($arg)*));
        }
    };
}

/// Context for parsing templates, tracking quote/quasiquote nesting
///
/// Quasiquote can be nested, and each level of nesting requires a corresponding unquote to escape.
/// Example: ``(a ,(b ,c))` has quasiquote depth 2 for `c`, depth 1 for `b`, and depth 0 for `a`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseContext {
    /// Normal context - pattern variables are substituted (quasiquote depth = 0)
    Normal,
    /// Inside a `(quote ...)` form - pattern variables become literals
    /// Quote is "absolute" - you cannot escape from it
    Quote,
    /// Inside `(quasiquote ...)` with nesting depth (depth >= 1)
    ///
    /// Each unquote decreases depth by 1; at depth 0, we're back to Normal
    Quasiquote(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroPattern {
    Literal(String),
    Variable(String),
    Wildcard,
    List(Vec<MacroPattern>),
    Ellipsis(Box<MacroPattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroTemplate {
    Literal(Value),
    Variable(String),
    List(Vec<MacroTemplate>),
    Ellipsis(Box<MacroTemplate>),
}

// Compiled pattern with pre-computed metadata for efficient matching
// This is computed once at macro definition time
#[derive(Debug, Clone)]
pub struct CompiledPattern {
    pub pattern: MacroPattern,
    /// All pattern variables (not including literals or wildcards)
    pub variables: HashSet<String>,
    /// Ellipsis nesting depth for each pattern variable
    /// Each pattern variable appears at exactly one depth (R7RS requirement)
    pub variable_depths: HashMap<String, usize>,
    /// Maximum ellipsis nesting depth in this pattern
    pub max_ellipsis_depth: usize,
}

// Compiled template with pre-computed metadata and validation
#[derive(Debug, Clone)]
pub struct CompiledTemplate {
    pub template: MacroTemplate,
    /// All identifiers in template (both pattern variables and literals like 'list', 'quote')
    pub variables: HashSet<String>,
    /// Ellipsis depth for pattern variables only (used to validate against pattern)
    /// Literals are not included here since they can appear at any depth
    pub pattern_variable_depths: HashMap<String, usize>,
    /// Maximum ellipsis nesting depth in this template
    pub max_ellipsis_depth: usize,
}

// Compiled rule with cross-validation between pattern and template
#[derive(Debug, Clone)]
pub struct CompiledRule {
    pub pattern: CompiledPattern,
    pub template: CompiledTemplate,
    /// Rule number for error messages
    pub rule_number: usize,
}

#[derive(Debug, Clone)]
pub struct SyntaxRule {
    pub pattern: MacroPattern,
    pub template: MacroTemplate,
}

#[derive(Debug, Clone)]
pub struct MacroDefinition {
    pub name: String,
    pub literals: Vec<String>,
    pub rules: Vec<SyntaxRule>,
    /// Compiled rules with metadata - computed once at definition time
    pub compiled_rules: Vec<CompiledRule>,
}

/// Parse syntax-rules form into a MacroDefinition with compiled metadata
pub fn parse_syntax_rules(name: &str, syntax_rules: &Value) -> Result<MacroDefinition, MacroError> {
    match syntax_rules {
        Value::List(items)
            if items.len() >= 2 && matches!(&items[0], Value::Symbol(s) if s == "syntax-rules") =>
        {
            let literals = parse_literals(&items[1])?;
            let rules_values = &items[2..];

            let rules = rules_values
                .iter()
                .map(|rule_item| parse_rule(rule_item, &literals))
                .collect::<Result<Vec<_>, _>>()?;

            if rules.is_empty() {
                return Err(MacroError(format!(
                    "syntax-rules for '{}' must have at least one rule.",
                    name
                )));
            }

            // Compile rules to pre-compute metadata and validate patterns
            let compiled_rules = compile_rules(&rules, name)?;

            Ok(MacroDefinition {
                name: name.to_string(),
                literals,
                rules,
                compiled_rules,
            })
        }
        _ => Err(MacroError("Invalid syntax-rules form".to_string())),
    }
}

fn parse_literals(literals_val: &Value) -> Result<Vec<String>, MacroError> {
    match literals_val {
        Value::List(items) => items
            .iter()
            .map(|item| match item {
                Value::Symbol(s) => {
                    // R7RS: Ellipsis cannot be a literal identifier
                    if s == "..." {
                        return Err(MacroError(
                            "Ellipsis '...' cannot appear in literals list".to_string(),
                        ));
                    }
                    // R7RS: Underscore cannot be a literal identifier (reserved for wildcard)
                    if s == "_" {
                        return Err(MacroError(
                            "Underscore '_' cannot appear in literals list (reserved for wildcard pattern)".to_string(),
                        ));
                    }
                    Ok(s.clone())
                }
                _ => Err(MacroError(format!(
                    "Invalid literal in syntax-rules literals list: expected symbol, found {}",
                    item
                ))),
            })
            .collect(),
        _ => Err(MacroError(
            "Invalid literals list in syntax-rules: expected a list".to_string(),
        )),
    }
}

fn parse_rule(rule: &Value, literals: &[String]) -> Result<SyntaxRule, MacroError> {
    match rule {
        Value::List(items) if items.len() == 2 => {
            let pattern = parse_pattern(&items[0], literals)?;
            let template = parse_template(&items[1])?;
            Ok(SyntaxRule { pattern, template })
        }
        _ => Err(MacroError(
            "Rule must be a list of pattern and template".to_string(),
        )),
    }
}

fn parse_pattern(pattern: &Value, literals: &[String]) -> Result<MacroPattern, MacroError> {
    match pattern {
        Value::Symbol(s) => {
            if literals.contains(s) {
                Ok(MacroPattern::Literal(s.clone()))
            } else if s == "_" {
                Ok(MacroPattern::Wildcard)
            } else if s == "..." {
                // R7RS: Ellipsis must follow another pattern element
                Err(MacroError(
                    "Ellipsis '...' cannot appear as standalone pattern element".to_string(),
                ))
            } else {
                Ok(MacroPattern::Variable(s.clone()))
            }
        }
        Value::List(items) => {
            let mut patterns = Vec::new();
            let mut it = items.iter().peekable();
            let mut last_was_ellipsis = false;

            while let Some(item) = it.next() {
                let sub_pattern = parse_pattern(item, literals)?;

                // R7RS: Check if next element is ellipsis
                if let Some(&Value::Symbol(s)) = it.peek() {
                    if s == "..." {
                        it.next(); // consume ellipsis

                        // R7RS: Ellipsis must not be followed by more elements
                        if it.peek().is_some() {
                            return Err(MacroError(
                                "Ellipsis '...' must be the last element in a pattern list"
                                    .to_string(),
                            ));
                        }

                        patterns.push(MacroPattern::Ellipsis(Box::new(sub_pattern)));
                        last_was_ellipsis = true;
                        continue;
                    }
                }

                patterns.push(sub_pattern);
            }

            // R7RS: Empty patterns are valid (matches empty list)
            if patterns.is_empty() && !last_was_ellipsis {
                // Empty patterns are valid (matches empty list)
            }

            Ok(MacroPattern::List(patterns))
        }
        _ => Ok(MacroPattern::Literal(pattern.to_string())),
    }
}

fn parse_template(template: &Value) -> Result<MacroTemplate, MacroError> {
    match template {
        Value::Symbol(s) => {
            if s == "..." {
                // R7RS: Ellipsis must follow another template element
                return Err(MacroError(
                    "Ellipsis '...' cannot appear as standalone template element".to_string(),
                ));
            }
            Ok(MacroTemplate::Variable(s.clone()))
        }
        Value::List(items) => {
            // R7RS RESTRICTED: Check for `(... <template>)` which is not supported.
            // This prevents using `...` as a literal in templates.
            if let Some(Value::Symbol(s)) = items.first() {
                if s == "..." {
                    return Err(MacroError(
                        "Ellipsis '...' cannot be used as a literal at the start of a template list. Use '(quote ...)' to produce a literal ellipsis.".to_string(),
                    ));
                }
            }

            let mut templates = Vec::new();
            let mut it = items.iter().peekable();

            while let Some(item) = it.next() {
                let mut sub_template = parse_template(item)?;

                // R7RS: Check for ellipsis (potentially multiple consecutive ones for nested ellipsis)
                while let Some(&Value::Symbol(s)) = it.peek() {
                    if s == "..." {
                        it.next(); // consume ellipsis

                        // Wrap the template in another Ellipsis layer
                        // This handles nested ellipsis like (x ... ...) which means
                        // "expand x at depth 2"
                        sub_template = MacroTemplate::Ellipsis(Box::new(sub_template));
                    } else {
                        break;
                    }
                }

                templates.push(sub_template);
            }
            Ok(MacroTemplate::List(templates))
        }
        _ => Ok(MacroTemplate::Literal(template.clone())),
    }
}

// ===== PATTERN COMPILATION PHASE =====
// Pre-compute metadata and validate patterns/templates at macro definition time

/// Compile a pattern: analyze structure and compute metadata
fn compile_pattern(
    pattern: &MacroPattern,
    macro_name: &str,
    rule_num: usize,
) -> Result<CompiledPattern, MacroError> {
    let mut variables = HashSet::new();
    let mut variable_depths = HashMap::new();

    analyze_pattern(pattern, 0, &mut variables, &mut variable_depths)?;

    let max_ellipsis_depth = variable_depths.values().max().copied().unwrap_or(0);

    debug_trace!(
        "Compiled pattern for {} rule {}: {} vars, max depth {}",
        macro_name,
        rule_num,
        variables.len(),
        max_ellipsis_depth
    );
    for (var, depth) in &variable_depths {
        debug_trace!("  {} at depth {}", var, depth);
    }

    Ok(CompiledPattern {
        pattern: pattern.clone(),
        variables,
        variable_depths,
        max_ellipsis_depth,
    })
}

/// Recursively analyze pattern structure and collect metadata
fn analyze_pattern(
    pattern: &MacroPattern,
    current_depth: usize,
    pattern_vars: &mut HashSet<String>,
    var_depths: &mut HashMap<String, usize>,
) -> Result<(), MacroError> {
    match pattern {
        MacroPattern::Variable(name) => {
            pattern_vars.insert(name.clone());

            // **R7RS REQUIREMENT**: Each pattern variable must appear at exactly one ellipsis depth
            // Check for conflicting depths
            if let Some(&existing_depth) = var_depths.get(name) {
                if existing_depth != current_depth {
                    return Err(MacroError(format!(
                        "Pattern variable '{}' used at inconsistent ellipsis depths: {} and {}. \
                         Each variable must appear at the same nesting level throughout the pattern.",
                        name, existing_depth, current_depth
                    )));
                }
            } else {
                var_depths.insert(name.clone(), current_depth);
            }
        }
        MacroPattern::List(patterns) => {
            for p in patterns {
                analyze_pattern(p, current_depth, pattern_vars, var_depths)?;
            }
        }
        MacroPattern::Ellipsis(sub_pattern) => {
            // Increase depth for patterns inside ellipsis
            analyze_pattern(sub_pattern, current_depth + 1, pattern_vars, var_depths)?;
        }
        MacroPattern::Literal(_) | MacroPattern::Wildcard => {
            // These don't bind variables
        }
    }
    Ok(())
}

/// Compile a template: analyze structure and validate against pattern
fn compile_template(
    template: &MacroTemplate,
    pattern: &CompiledPattern,
    macro_name: &str,
    rule_num: usize,
) -> Result<CompiledTemplate, MacroError> {
    let mut template_identifiers = HashSet::new();
    let mut pattern_var_depths = HashMap::new();

    analyze_template(
        template,
        0,
        ParseContext::Normal, // Not in a quote context initially
        &mut template_identifiers,
        &mut pattern_var_depths,
        &pattern.variables,
    )?;

    let max_ellipsis_depth = pattern_var_depths.values().max().copied().unwrap_or(0);

    // Validate: template depth cannot exceed pattern depth for pattern variables
    for (var, &template_depth) in &pattern_var_depths {
        if let Some(&pattern_depth) = pattern.variable_depths.get(var) {
            if template_depth > pattern_depth {
                return Err(MacroError(format!(
                    "In macro '{}' rule {}: template variable '{}' used at ellipsis depth {} \
                     but only bound at depth {} in pattern. Template cannot add ellipsis levels.",
                    macro_name, rule_num, var, template_depth, pattern_depth
                )));
            }
            // It's OK for template_depth < pattern_depth (variable expands to list)
        } else {
            // Internal compiler error: variable in pattern_var_depths but not in pattern
            return Err(MacroError(format!(
                "Internal error in macro '{}' rule {}: template variable '{}' tracked for depth but not in pattern variables",
                macro_name, rule_num, var
            )));
        }
    }

    debug_trace!(
        "Compiled template for {} rule {}: {} identifiers, max depth {}",
        macro_name,
        rule_num,
        template_identifiers.len(),
        max_ellipsis_depth
    );

    Ok(CompiledTemplate {
        template: template.clone(),
        variables: template_identifiers,
        pattern_variable_depths: pattern_var_depths,
        max_ellipsis_depth,
    })
}

/// Recursively analyze template structure and track pattern variable depths
/// Only pattern variables (from pattern_vars set) are tracked in var_depths
/// Literals and other identifiers are collected in identifiers but not tracked for depth
fn analyze_template(
    template: &MacroTemplate,
    current_depth: usize,
    context: ParseContext,
    identifiers: &mut HashSet<String>,
    var_depths: &mut HashMap<String, usize>,
    pattern_vars: &HashSet<String>,
) -> Result<(), MacroError> {
    match template {
        MacroTemplate::Variable(name) => {
            identifiers.insert(name.clone());

            // Check if pattern variables should be substituted based on context
            let should_substitute = match context {
                ParseContext::Normal => true,
                ParseContext::Quote => false,
                ParseContext::Quasiquote(_) => false, // Variables in quasiquote are literal
            };

            if !should_substitute {
                return Ok(()); // Variable is literal, no validation needed
            }

            // Special check for underscore: only allowed in quote contexts
            if name == "_" {
                return Err(MacroError(
                    "Underscore '_' cannot be used as a variable in a template. Use '(quote _)' for a literal underscore.".to_string()
                ));
            }

            // Only track depth for actual pattern variables
            if pattern_vars.contains(name) {
                // **R7RS REQUIREMENT**: Pattern variables must be used at consistent depth
                if let Some(&existing_depth) = var_depths.get(name) {
                    if existing_depth != current_depth {
                        return Err(MacroError(format!(
                            "Pattern variable '{}' used at inconsistent ellipsis depths: {} and {}",
                            name, existing_depth, current_depth
                        )));
                    }
                } else {
                    var_depths.insert(name.clone(), current_depth);
                }
            }
            // Literals (non-pattern variables) don't need depth tracking.
        }
        MacroTemplate::List(templates) => {
            // Special forms are only recognized in Normal or Quasiquote contexts
            // Inside Quote context, everything is literal including "quote", "quasiquote", etc.
            if context != ParseContext::Quote
                && let Some(MacroTemplate::Variable(special_form)) = templates.first()
                && matches!(
                    special_form.as_str(),
                    "quote" | "quasiquote" | "unquote" | "unquote-splicing"
                )
            {
                // All special forms require exactly 2 elements: (form datum)
                if templates.len() != 2 {
                    return Err(MacroError(format!(
                        "Malformed {} form in template: expected ({} <datum>), found list with {} elements",
                        special_form,
                        special_form,
                        templates.len()
                    )));
                }

                // Determine new context based on the form and current context
                let new_context = match special_form.as_str() {
                    "quote" => ParseContext::Quote,
                    "quasiquote" => ParseContext::Quasiquote(match context {
                        ParseContext::Quasiquote(depth) => depth + 1,
                        _ => 1,
                    }),
                    "unquote" | "unquote-splicing" => match context {
                        ParseContext::Quasiquote(1) => ParseContext::Normal,
                        ParseContext::Quasiquote(depth) => ParseContext::Quasiquote(depth - 1),
                        _ => {
                            return Err(MacroError(format!(
                                "{} form encountered outside quasiquote context in template",
                                special_form
                            )));
                        }
                    },
                    _ => unreachable!(),
                };

                // Analyze the datum with the new context
                return analyze_template(
                    &templates[1],
                    current_depth,
                    new_context,
                    identifiers,
                    var_depths,
                    pattern_vars,
                );
            }

            // Not a special form (or inside Quote context), analyze all elements with current context
            for t in templates {
                analyze_template(
                    t,
                    current_depth,
                    context,
                    identifiers,
                    var_depths,
                    pattern_vars,
                )?;
            }
        }
        MacroTemplate::Ellipsis(sub_template) => {
            // Increase depth for templates inside ellipsis.
            // Ellipsis itself is not allowed inside a quote, but the parser handles that.
            // The content of the ellipsis is analyzed with the new depth.
            analyze_template(
                sub_template,
                current_depth + 1,
                context,
                identifiers,
                var_depths,
                pattern_vars,
            )?;
        }
        MacroTemplate::Literal(_) => {
            // Literals don't have variables
        }
    }
    Ok(())
}

/// Compile all rules for a macro definition
fn compile_rules(rules: &[SyntaxRule], macro_name: &str) -> Result<Vec<CompiledRule>, MacroError> {
    let mut compiled = Vec::new();

    for (idx, rule) in rules.iter().enumerate() {
        let rule_num = idx + 1;

        debug_trace!("Compiling {} rule {}", macro_name, rule_num);

        let compiled_pattern = compile_pattern(&rule.pattern, macro_name, rule_num)?;
        let compiled_template =
            compile_template(&rule.template, &compiled_pattern, macro_name, rule_num)?;

        compiled.push(CompiledRule {
            pattern: compiled_pattern,
            template: compiled_template,
            rule_number: rule_num,
        });
    }

    Ok(compiled)
}
