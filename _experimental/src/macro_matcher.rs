// Macro pattern matching - runtime matching of macro use-sites against patterns
//
// This module handles pattern matching for R7RS syntax-rules macros at expansion time.
// It takes compiled patterns (from macro_compiler.rs) and matches them against input forms,
// producing hierarchical binding structures that capture the matched values at correct depths.
//
// **R7RS COMPLIANCE STATUS:**
//
// **CRITICAL GAP - NESTED ELLIPSIS:**
// - This matcher correctly handles nested ellipsis patterns (e.g., `((x ...) ...)`).
// - It produces the correct hierarchical `Binding::Repeated` structure.
// - However, the `macro_expander` CANNOT consume this nested structure, making the
//   overall feature non-functional. This is a major R7RS deviation.
//
// **PARTIALLY IMPLEMENTED R7RS FEATURES:**
// - ✓ Pattern variable matching: Variables bind to values at their declared ellipsis depth.
// - ✓ Ellipsis patterns: `(pattern ...)` for zero-or-more repetition with correct binding depth.
// - ✓ Nested ellipsis matching: `((x ...) ...)` is matched into a correct, but unusable, nested `Binding` structure.
// - ✓ Hierarchical bindings: `Direct` (depth 0) and `Repeated` (depth N>0) with proper nesting.
//
// **R7RS RESTRICTED** (Inherited from macro_compiler.rs):
// - ✗ Vector patterns: `#(pattern ...)` - project has no vector support.
// - ✗ Improper list patterns: `(a b . rest)` - project only supports proper lists.
//
// **IMPLEMENTATION NOTES:**
// - The matcher's output (the `Binding` tree) is correct even for nested ellipsis. The failure
//   occurs downstream in the expander, which cannot interpret this correct structure.

use crate::macro_compiler::{CompiledPattern, MacroPattern};
use crate::value::Value;
use std::collections::HashMap;

// Global debug flag - set via environment variable MACRO_DEBUG=1
static DEBUG: std::sync::LazyLock<bool> =
    std::sync::LazyLock::new(|| std::env::var("MACRO_DEBUG").is_ok());

macro_rules! debug_trace {
    ($($arg:tt)*) => {
        if *DEBUG {
            eprintln!("[MATCHER DEBUG] {}", format!($($arg)*));
        }
    };
}

/// Hierarchical binding structure for pattern variables
///
/// This represents values bound to pattern variables at different ellipsis nesting depths.
/// The structure mirrors the ellipsis nesting in the pattern.
///
/// Example:
///
/// - Pattern `(x)` matching `(5)` → x: Direct(5)
/// - Pattern `(x ...)` matching `(1 2 3)` → x: Repeated{count: 3, elements: [Direct(1), Direct(2), Direct(3)]}
/// - Pattern `((x ...) ...)` matching `((1 2) (3 4))`:
///
/// ```text
/// x: Repeated{count: 2, elements: [
///   Repeated{count: 2, elements: [Direct(1), Direct(2)]},
///   Repeated{count: 2, elements: [Direct(3), Direct(4)]}
/// ]}
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    /// Direct value binding - variable matched a single value (depth 0)
    Direct(Value),

    /// Repeated binding - variable appeared in ellipsis pattern (depth N > 0)
    /// Each element is a Binding at depth N-1
    Repeated {
        count: usize,
        elements: Vec<Binding>,
    },
}

impl Binding {
    /// Format binding for debug output - compact representation
    pub fn debug_format(&self) -> String {
        match self {
            Binding::Direct(v) => format!("Direct({})", value_compact(v)),
            Binding::Repeated { count, elements } => {
                let elem_strs: Vec<_> = elements.iter().map(|e| e.debug_format()).collect();
                format!("Repeated[{}]({})", count, elem_strs.join(", "))
            }
        }
    }

    /// Get the repetition count (0 for Direct, count for Repeated)
    pub fn repetition_count(&self) -> usize {
        match self {
            Binding::Direct(_) => 0,
            Binding::Repeated { count, .. } => *count,
        }
    }

    /// Get the i-th element (for Repeated bindings)
    pub fn get_element(&self, index: usize) -> Option<&Binding> {
        match self {
            Binding::Direct(_) => None,
            Binding::Repeated { elements, .. } => elements.get(index),
        }
    }

    /// Convert to direct Value (for Direct bindings)
    pub fn to_value(&self) -> Option<&Value> {
        match self {
            Binding::Direct(v) => Some(v),
            Binding::Repeated { .. } => None,
        }
    }
}

/// Context holding all pattern variable bindings after successful match
#[derive(Debug, Clone)]
pub struct MatchContext {
    pub bindings: HashMap<String, Binding>,
}

impl Default for MatchContext {
    fn default() -> Self {
        Self::new()
    }
}

impl MatchContext {
    pub fn new() -> Self {
        MatchContext {
            bindings: HashMap::new(),
        }
    }

    pub fn bind(&mut self, name: String, binding: Binding) {
        debug_trace!("  Binding {} = {}", name, binding.debug_format());
        self.bindings.insert(name, binding);
    }

    pub fn get(&self, name: &str) -> Option<&Binding> {
        self.bindings.get(name)
    }
}

/// Result of pattern matching attempt
pub enum MatchResult {
    Success(MatchContext),
    Failure(String),
}

/// Match a compiled pattern against an input value
///
/// This is the main entry point for pattern matching. It uses the compiled metadata
/// to guide the matching process and construct correct binding hierarchies.
pub fn match_pattern(
    compiled: &CompiledPattern,
    value: &Value,
    literals: &[String],
) -> MatchResult {
    debug_trace!(
        "=== MATCHING PATTERN (max_depth={}) ===",
        compiled.max_ellipsis_depth
    );
    debug_trace!("Input: {}", value_compact(value));

    let mut context = MatchContext::new();

    if match_pattern_internal(
        &compiled.pattern,
        value,
        &mut context,
        literals,
        0,
        compiled,
    ) {
        debug_trace!("=== MATCH SUCCESS ===");
        for (var, binding) in &context.bindings {
            debug_trace!("  {} = {}", var, binding.debug_format());
        }
        MatchResult::Success(context)
    } else {
        debug_trace!("=== MATCH FAILED ===");
        MatchResult::Failure("Pattern did not match input".to_string())
    }
}

/// Internal pattern matching with depth tracking
///
/// current_depth tracks how many ellipsis layers we're inside (starts at 0)
/// compiled provides metadata about expected depths for each variable
fn match_pattern_internal(
    pattern: &MacroPattern,
    value: &Value,
    context: &mut MatchContext,
    literals: &[String],
    current_depth: usize,
    compiled: &CompiledPattern,
) -> bool {
    debug_trace!(
        "match_pattern_internal: {:?} against {} (depth {})",
        pattern,
        value_compact(value),
        current_depth
    );

    match (pattern, value) {
        // Literal must match exactly
        (MacroPattern::Literal(lit), Value::Symbol(s)) if lit == s => true,
        (MacroPattern::Literal(lit), other) if lit == &other.to_string() => true,

        // Wildcard matches anything without binding
        (MacroPattern::Wildcard, _) => true,

        // Variable matches any value and binds it
        // **R7RS COMPLIANCE:** Variables always bind to the value at their current depth.
        // - At depth 0: Creates Direct binding in main context
        // - At depth N > 0: Creates Direct binding in temp context (per-iteration)
        //   The ellipsis handler collects these Direct bindings and wraps them into Repeated
        (MacroPattern::Variable(name), val) => {
            let expected_depth = compiled.variable_depths.get(name).copied().unwrap_or(0);

            // Verify we're at the correct depth for this variable
            if expected_depth == current_depth {
                // Correct depth - bind the value directly
                let binding = Binding::Direct(val.clone());
                context.bind(name.clone(), binding);
                true
            } else {
                // Depth mismatch - this is a pattern structure error
                debug_trace!(
                    "ERROR: Variable {} at depth {} but expected depth {}",
                    name,
                    current_depth,
                    expected_depth
                );
                false
            }
        }

        // List pattern must match list value
        (MacroPattern::List(patterns), Value::List(values)) => {
            match_list_pattern(patterns, values, context, literals, current_depth, compiled)
        }

        // No match
        _ => false,
    }
}

/// Match a list of patterns against a list of values
///
/// This handles both regular patterns and ellipsis patterns.
/// The key challenge is correctly handling ellipsis patterns which consume
/// multiple values and create Repeated bindings.
fn match_list_pattern(
    patterns: &[MacroPattern],
    values: &[Value],
    context: &mut MatchContext,
    literals: &[String],
    current_depth: usize,
    compiled: &CompiledPattern,
) -> bool {
    debug_trace!(
        "match_list_pattern: {} patterns vs {} values (depth {})",
        patterns.len(),
        values.len(),
        current_depth
    );

    let mut pattern_idx = 0;
    let mut value_idx = 0;

    while pattern_idx < patterns.len() {
        let pattern = &patterns[pattern_idx];

        // Check if this is an ellipsis pattern
        if let MacroPattern::Ellipsis(sub_pattern) = pattern {
            debug_trace!("  Found ellipsis pattern at index {}", pattern_idx);

            // Ellipsis must be the last pattern (enforced by compiler)
            // It matches zero or more remaining values
            let remaining_values = &values[value_idx..];

            return match_ellipsis_pattern(
                sub_pattern,
                remaining_values,
                context,
                literals,
                current_depth,
                compiled,
            );
        }

        // Regular pattern - must match exactly one value
        if value_idx >= values.len() {
            debug_trace!("  Not enough values for pattern at index {}", pattern_idx);
            return false;
        }

        if !match_pattern_internal(
            pattern,
            &values[value_idx],
            context,
            literals,
            current_depth,
            compiled,
        ) {
            debug_trace!(
                "  Pattern {} failed to match value {}",
                pattern_idx,
                value_idx
            );
            return false;
        }

        pattern_idx += 1;
        value_idx += 1;
    }

    // All patterns consumed - all values must be consumed too
    if value_idx != values.len() {
        debug_trace!(
            "  Extra values remaining: {} of {}",
            values.len() - value_idx,
            values.len()
        );
        return false;
    }

    true
}

/// Match an ellipsis pattern against multiple values
///
/// This is where we construct Repeated bindings with the correct hierarchical structure.
/// Each variable in the ellipsis sub-pattern gets ONE Repeated binding containing all
/// the matched instances.
///
/// **R7RS COMPLIANCE:** This correctly handles variables at all ellipsis depths.
/// Variables at depth N are bound to Repeated bindings containing depth N-1 elements.
fn match_ellipsis_pattern(
    sub_pattern: &MacroPattern,
    values: &[Value],
    context: &mut MatchContext,
    literals: &[String],
    current_depth: usize,
    compiled: &CompiledPattern,
) -> bool {
    debug_trace!(
        "match_ellipsis_pattern: matching {} values at depth {}",
        values.len(),
        current_depth + 1
    );

    // Collect all variables that will be bound in this ellipsis
    let vars = collect_pattern_variables(sub_pattern);
    debug_trace!("  Variables in ellipsis: {:?}", vars);

    // Match each value against the sub-pattern, collecting bindings
    let mut all_bindings: HashMap<String, Vec<Binding>> = HashMap::new();
    for var in &vars {
        all_bindings.insert(var.clone(), Vec::new());
    }

    for (i, value) in values.iter().enumerate() {
        debug_trace!("  Matching iteration {} of {}", i, values.len());

        // Create temporary context for this iteration
        let mut temp_context = MatchContext::new();

        if !match_pattern_internal(
            sub_pattern,
            value,
            &mut temp_context,
            literals,
            current_depth + 1,
            compiled,
        ) {
            debug_trace!("  Iteration {} failed to match", i);
            return false;
        }

        // Collect bindings from this iteration
        for var in &vars {
            if let Some(binding) = temp_context.get(var) {
                all_bindings.get_mut(var).unwrap().push(binding.clone());
            } else {
                debug_trace!("  ERROR: Variable {} not bound in iteration {}", var, i);
                return false;
            }
        }
    }

    // Now create Repeated bindings for each variable
    for var in &vars {
        let elements = all_bindings.get(var).unwrap();
        let binding = Binding::Repeated {
            count: elements.len(),
            elements: elements.clone(),
        };
        context.bind(var.clone(), binding);
    }

    true
}

/// Collect all variable names from a pattern (excluding literals and wildcards)
fn collect_pattern_variables(pattern: &MacroPattern) -> Vec<String> {
    let mut vars = Vec::new();
    collect_pattern_variables_impl(pattern, &mut vars);
    vars
}

fn collect_pattern_variables_impl(pattern: &MacroPattern, vars: &mut Vec<String>) {
    match pattern {
        MacroPattern::Variable(name) => {
            if !vars.contains(name) {
                vars.push(name.clone());
            }
        }
        MacroPattern::List(patterns) => {
            for p in patterns {
                collect_pattern_variables_impl(p, vars);
            }
        }
        MacroPattern::Ellipsis(sub_pattern) => {
            collect_pattern_variables_impl(sub_pattern, vars);
        }
        MacroPattern::Literal(_) | MacroPattern::Wildcard => {}
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
