// Macro expansion system - implements R7RS syntax-rules macros
//
// ## Architecture:
// This module implements a complete R7RS-compliant macro system with:
// 1. **Prelude Loading**: Standard derived expressions loaded from prelude/macros.scm
// 2. **Stabilization-based Expansion**: Expands until pre-expansion == post-expansion
// 3. **Ellipsis Support**: Pattern matching with (...) for variable arguments
// 4. **User-defined Macros**: Full define-syntax and syntax-rules support
//
// The macro prelude (prelude/macros.scm) contains all R7RS derived expressions:
// and, or, when, unless, cond, case, let, let*, do - these are essential for
// R7RS compliance and are automatically loaded at startup.
//
// ## R7RS Deviations and Limitations:
//
// **Missing Features:**
// - `let-syntax` and `letrec-syntax` (local macro bindings) - only top-level `define-syntax` supported
// - Proper macro hygiene - no identifier renaming, potential variable capture issues
// - Underscore (`_`) wildcard patterns - not implemented
// - Vector patterns `#(pattern ...)` - vectors not supported in core language
// - Improper list patterns (dotted pairs) - core language uses proper lists only
// - Nested ellipsis patterns - complex repetition not supported
// - Pattern variables in different ellipsis depths - binding complexity not handled
//
// **Partial Implementations:**
// - Ellipsis patterns `...` - basic support, but template expansion has bugs
// - Multiple rules per macro - supported but limited pattern matching
//
// **TODO Items for Full R7RS Compliance:**
// - Implement proper hygiene with gensym and renaming
// - Support `let-syntax` and `letrec-syntax`
// - Handle nested ellipsis patterns correctly
// - Add underscore wildcard pattern support
// - Improve error messages with better source location tracking

use crate::value::{Environment, Value};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// R7RS derived expressions implemented as macros in prelude/macros.scm
/// These forms are handled by the macro system, not the compiler
pub const STANDARD_DERIVED_EXPRESSIONS: &[&str] = &[
    "and", "or", // Logical operators (R7RS 4.2.1)
    "when", "unless", // Simple conditionals (R7RS 4.2.6)
    "cond", "case", // Multi-way conditionals (R7RS 4.2.1, 4.2.5)
    "let", "let*", // Local binding forms (R7RS 4.2.2)
    "do",   // Iteration form (R7RS 4.2.4)
];

#[derive(Debug, Clone)]
pub struct MacroError(pub String);

impl std::fmt::Display for MacroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Macro error: {}", self.0)
    }
}

impl std::error::Error for MacroError {}

/// A pattern in a syntax-rules macro
///
/// **R7RS Deviations:** Missing support for:
/// - Underscore wildcard patterns (`_`)
/// - Vector patterns (`#(pattern ...)`)
/// - Improper list patterns (dotted pairs)
/// - Nested ellipsis with different binding depths
#[derive(Debug, Clone, PartialEq)]
pub enum MacroPattern {
    /// Literal identifier that must match exactly
    Literal(String),
    /// Pattern variable that binds to any expression
    Variable(String),
    /// List pattern containing sub-patterns
    List(Vec<MacroPattern>),
    /// Ellipsis pattern for zero or more repetitions
    /// **TODO:** Implement proper ellipsis nesting and complex binding patterns
    Ellipsis(Box<MacroPattern>),
    // TODO: Add support for R7RS features:
    // Underscore,  // Wildcard pattern _
    // Vector(Vec<MacroPattern>),  // Vector patterns #(...)
    // DottedList(Vec<MacroPattern>, Box<MacroPattern>),  // Improper lists
}

/// A template for macro expansion
///
/// **R7RS Deviations:** Missing support for:
/// - Vector templates (`#(template ...)`)
/// - Improper list templates (dotted pairs)
/// - Proper hygiene (identifier renaming)
#[derive(Debug, Clone, PartialEq)]
pub enum MacroTemplate {
    /// Literal value to insert
    Literal(Value),
    /// Pattern variable to substitute
    Variable(String),
    /// List template containing sub-templates
    List(Vec<MacroTemplate>),
    /// Ellipsis template for expanding repeated patterns
    /// **BUG:** Current implementation has issues with proper expansion
    Ellipsis(Box<MacroTemplate>),
    // TODO: Add support for R7RS features:
    // Vector(Vec<MacroTemplate>),  // Vector templates #(...)
    // DottedList(Vec<MacroTemplate>, Box<MacroTemplate>),  // Improper lists
}

/// A single syntax rule (pattern -> template)
#[derive(Debug, Clone)]
pub struct SyntaxRule {
    pub pattern: MacroPattern,
    pub template: MacroTemplate,
}

/// A macro definition
#[derive(Debug, Clone)]
pub struct MacroDefinition {
    pub name: String,
    pub literals: Vec<String>,
    pub rules: Vec<SyntaxRule>,
}

/// Pattern variable bindings during macro expansion
pub type PatternBindings = HashMap<String, Vec<Value>>;

/// Per-expression macro expander with stabilization-based expansion
///
/// **R7RS Limitations:**
/// - No proper hygiene system (identifiers can be captured)
/// - Only supports top-level `define-syntax`, no `let-syntax`/`letrec-syntax`
/// - Simple gensym counter, not a proper renaming system
pub struct MacroExpander {
    macros: HashMap<String, MacroDefinition>,
    /// Simple gensym counter for hygiene (not yet implemented)
    #[allow(dead_code)]
    gensym_counter: usize,
    /// Symbols that are known to be macros
    known_macro_symbols: std::collections::HashSet<String>,
    /// Track macro symbols emitted during current expansion for short-circuit optimization
    emitted_macro_symbols: std::collections::HashSet<String>,
}

impl MacroExpander {
    pub fn new(_environment: Rc<Environment>) -> Self {
        MacroExpander {
            macros: HashMap::new(),
            gensym_counter: 0,
            known_macro_symbols: HashSet::new(),
            emitted_macro_symbols: HashSet::new(),
        }
    }

    /// Expand macros in the given AST using per-expression stabilization
    pub fn expand(&mut self, ast: &Value) -> Result<Value, MacroError> {
        self.expand_until_stable(ast)
    }

    /// Expand an expression until it stabilizes (pre-expansion == post-expansion)
    fn expand_until_stable(&mut self, ast: &Value) -> Result<Value, MacroError> {
        const MAX_EXPANSIONS: usize = 100; // Safety limit for infinite expansion
        let mut current = ast.clone();
        let mut expansion_count = 0;

        loop {
            // Clear emitted macro symbols for this expansion
            self.emitted_macro_symbols.clear();

            let before_expansion = current.clone();
            current = self.expand_once(&current)?;

            expansion_count += 1;
            if expansion_count > MAX_EXPANSIONS {
                return Err(MacroError(format!(
                    "Expression expansion exceeded {} iterations. This suggests infinite macro expansion.",
                    MAX_EXPANSIONS
                )));
            }

            // Short-circuit: if no macro symbols were emitted, we're definitely done
            if self.emitted_macro_symbols.is_empty() {
                return Ok(current);
            }

            // Fall back to full AST comparison if macros were emitted
            if current == before_expansion {
                return Ok(current);
            }
        }
    }

    /// Perform one expansion pass on an expression
    fn expand_once(&mut self, ast: &Value) -> Result<Value, MacroError> {
        match ast {
            Value::List(items) if !items.is_empty() => {
                // Check if this is a define-syntax form
                if let Value::Symbol(name) = &items[0] {
                    if name == "define-syntax" {
                        return self.handle_define_syntax(items);
                    }

                    // **R7RS DEVIATION:** Block local macro definitions through let-syntax and letrec-syntax
                    if name == "let-syntax" || name == "letrec-syntax" {
                        return Err(MacroError(format!(
                            "R7RS DEVIATION: {} is not supported - only top-level define-syntax is implemented. \
                             Local macro bindings require environment extensions.",
                            name
                        )));
                    }

                    // **R7RS DEVIATION:** Check for other unsupported macro-related forms
                    if name == "syntax-case" || name == "syntax" || name == "quasisyntax" {
                        return Err(MacroError(format!(
                            "R7RS DEVIATION: {} is not supported - only syntax-rules macros are implemented",
                            name
                        )));
                    }

                    // Check if this is a macro invocation
                    if let Some(macro_def) = self.get_macro(name) {
                        return self.expand_macro(&macro_def, &items[1..]);
                    }
                }

                // Not a macro - recursively expand subexpressions in single pass. This is safe without depth tracking because depth tracking should be done in the parser.
                let expanded: Result<Vec<_>, _> =
                    items.iter().map(|item| self.expand_once(item)).collect();
                Ok(Value::List(expanded?))
            }
            // Other value types don't contain macros
            _ => Ok(ast.clone()),
        }
    }

    /// Handle define-syntax forms
    fn handle_define_syntax(&mut self, items: &[Value]) -> Result<Value, MacroError> {
        if items.len() != 3 {
            return Err(MacroError(
                "define-syntax requires exactly 2 arguments".to_string(),
            ));
        }

        let name = match &items[1] {
            Value::Symbol(s) => s.clone(),
            _ => {
                return Err(MacroError(
                    "define-syntax name must be a symbol".to_string(),
                ))
            }
        };

        let syntax_rules = &items[2];
        let macro_def = self.parse_syntax_rules(&name, syntax_rules)?;

        // Store the macro in the environment
        self.store_macro(macro_def)?;

        // Return unspecified value (define-syntax doesn't produce a value)
        Ok(Value::Unspecified)
    }

    /// Parse a syntax-rules form into a MacroDefinition
    fn parse_syntax_rules(
        &self,
        name: &str,
        syntax_rules: &Value,
    ) -> Result<MacroDefinition, MacroError> {
        match syntax_rules {
            Value::List(items) if items.len() >= 2 => {
                // Check that first element is 'syntax-rules'
                if let Value::Symbol(s) = &items[0] {
                    if s != "syntax-rules" {
                        return Err(MacroError("Expected syntax-rules".to_string()));
                    }
                } else {
                    return Err(MacroError("Expected syntax-rules".to_string()));
                }

                // Parse literals list
                let literals = self.parse_literals(&items[1])?;

                // Parse rules
                let mut rules = Vec::new();
                for rule_item in &items[2..] {
                    rules.push(self.parse_rule(rule_item)?);
                }

                if rules.is_empty() {
                    return Err(MacroError(
                        "syntax-rules must have at least one rule".to_string(),
                    ));
                }

                Ok(MacroDefinition {
                    name: name.to_string(),
                    literals,
                    rules,
                })
            }
            _ => Err(MacroError("Invalid syntax-rules form".to_string())),
        }
    }

    /// Parse the literals list from syntax-rules
    fn parse_literals(&self, literals: &Value) -> Result<Vec<String>, MacroError> {
        match literals {
            Value::List(items) => {
                let mut result = Vec::new();
                for item in items {
                    match item {
                        Value::Symbol(s) => result.push(s.clone()),
                        _ => return Err(MacroError("Literals must be symbols".to_string())),
                    }
                }
                Ok(result)
            }
            _ => Err(MacroError("Literals must be a list".to_string())),
        }
    }

    /// Parse a single rule from syntax-rules
    fn parse_rule(&self, rule: &Value) -> Result<SyntaxRule, MacroError> {
        match rule {
            Value::List(items) if items.len() == 2 => {
                let pattern = self.parse_pattern(&items[0])?;
                let template = self.parse_template(&items[1])?;
                Ok(SyntaxRule { pattern, template })
            }
            _ => Err(MacroError(
                "Rule must be a list of pattern and template".to_string(),
            )),
        }
    }

    /// Parse a pattern from a rule
    /// **R7RS Deviations:** Missing underscore wildcards, vector patterns, improper lists
    #[allow(clippy::only_used_in_recursion)]
    fn parse_pattern(&self, pattern: &Value) -> Result<MacroPattern, MacroError> {
        match pattern {
            Value::Symbol(s) => {
                // **R7RS DEVIATION:** No underscore wildcard support
                if s == "_" {
                    return Err(MacroError(
                        "R7RS DEVIATION: Underscore wildcard patterns (_) are not supported"
                            .to_string(),
                    ));
                }
                // Check if this is a literal or a pattern variable
                // For now, we'll implement this simply - anything in the literals list is literal
                Ok(MacroPattern::Variable(s.clone()))
            }
            Value::List(items) => {
                if items.is_empty() {
                    return Ok(MacroPattern::List(Vec::new()));
                }

                let mut patterns = Vec::new();
                let mut i = 0;
                while i < items.len() {
                    // Check for ellipsis patterns
                    if i + 1 < items.len() {
                        if let Value::Symbol(s) = &items[i + 1] {
                            if s == "..." {
                                // **BUG FIX:** (values ...) is valid - ellipsis can be second element
                                let sub_pattern = self.parse_pattern(&items[i])?;

                                // **R7RS DEVIATION:** Check for nested ellipsis patterns
                                // Note: Pattern variables at different ellipsis depths are inherently
                                // blocked by this check, since they require nested ellipsis patterns
                                // like ((x ...) ...) where x appears at depth 2
                                if self.contains_ellipsis(&sub_pattern) {
                                    return Err(MacroError(
                                        "R7RS DEVIATION: Nested ellipsis patterns not supported"
                                            .to_string(),
                                    ));
                                }

                                patterns.push(MacroPattern::Ellipsis(Box::new(sub_pattern)));
                                i += 2; // Skip the ... symbol
                                continue;
                            }
                        }
                    }

                    // Check for isolated ellipsis (error)
                    if let Value::Symbol(s) = &items[i] {
                        if s == "..." {
                            return Err(MacroError(
                                "Ellipsis (...) must follow a pattern element".to_string(),
                            ));
                        }
                    }

                    patterns.push(self.parse_pattern(&items[i])?);
                    i += 1;
                }
                Ok(MacroPattern::List(patterns))
            }
            _ => {
                // Literal values become literal patterns
                Ok(MacroPattern::Literal(format!("{}", pattern)))
            }
        }
    }

    /// Parse a template from a rule
    #[allow(clippy::only_used_in_recursion)]
    fn parse_template(&self, template: &Value) -> Result<MacroTemplate, MacroError> {
        match template {
            Value::Symbol(s) => Ok(MacroTemplate::Variable(s.clone())),
            Value::List(items) => {
                let mut templates = Vec::new();
                let mut i = 0;
                while i < items.len() {
                    if i + 1 < items.len() {
                        if let Value::Symbol(s) = &items[i + 1] {
                            if s == "..." {
                                // This is an ellipsis template
                                let sub_template = self.parse_template(&items[i])?;
                                templates.push(MacroTemplate::Ellipsis(Box::new(sub_template)));
                                i += 2; // Skip the ... symbol
                                continue;
                            }
                        }
                    }
                    templates.push(self.parse_template(&items[i])?);
                    i += 1;
                }
                Ok(MacroTemplate::List(templates))
            }
            _ => {
                // Literal values
                Ok(MacroTemplate::Literal(template.clone()))
            }
        }
    }

    /// Expand a macro with given arguments
    fn expand_macro(
        &mut self,
        macro_def: &MacroDefinition,
        args: &[Value],
    ) -> Result<Value, MacroError> {
        // Reconstruct the full macro call for pattern matching
        let mut full_call = vec![Value::Symbol(macro_def.name.clone())];
        full_call.extend_from_slice(args);

        // Convert to a single List value for pattern matching
        let call_as_list = Value::List(full_call);

        // Try each rule in order until one matches
        for rule in macro_def.rules.iter() {
            if let Ok(bindings) = self.match_pattern(
                &rule.pattern,
                std::slice::from_ref(&call_as_list),
                &macro_def.literals,
            ) {
                return self.instantiate_template(&rule.template, &bindings);
            }
        }

        Err(MacroError(format!(
            "No matching pattern for macro {} with {} arguments",
            macro_def.name,
            args.len()
        )))
    }

    /// Match a pattern against arguments
    fn match_pattern(
        &self,
        pattern: &MacroPattern,
        args: &[Value],
        literals: &[String],
    ) -> Result<PatternBindings, MacroError> {
        let mut bindings = HashMap::new();
        self.match_pattern_recursive(pattern, args, literals, &mut bindings)?;
        Ok(bindings)
    }

    /// Match a list pattern against list items, handling ellipsis patterns
    fn match_list_pattern(
        &self,
        patterns: &[MacroPattern],
        items: &[Value],
        literals: &[String],
        bindings: &mut PatternBindings,
    ) -> Result<(), MacroError> {
        let mut pattern_idx = 0;
        let mut item_idx = 0;

        while pattern_idx < patterns.len() && item_idx < items.len() {
            match &patterns[pattern_idx] {
                MacroPattern::Ellipsis(sub_pattern) => {
                    // Handle ellipsis pattern - match zero or more
                    let mut ellipsis_matches = Vec::new();

                    // Try to match as many items as possible with the sub-pattern
                    while item_idx < items.len() {
                        let mut sub_bindings = HashMap::new();

                        match self.match_pattern_recursive(
                            sub_pattern,
                            std::slice::from_ref(&items[item_idx]),
                            literals,
                            &mut sub_bindings,
                        ) {
                            Ok(()) => {
                                // For simple variable patterns, collect the matched item
                                if let MacroPattern::Variable(_) = sub_pattern.as_ref() {
                                    ellipsis_matches.push(items[item_idx].clone());
                                } else {
                                    // For complex patterns, use sub-bindings
                                    for (var, values) in sub_bindings {
                                        bindings.entry(var).or_default().extend(values);
                                    }
                                }

                                item_idx += 1;
                            }
                            Err(_) => {
                                // Stop ellipsis matching when pattern no longer matches
                                break;
                            }
                        }
                    }

                    // For simple variable ellipsis patterns, bind the list of matches
                    if let MacroPattern::Variable(var_name) = sub_pattern.as_ref() {
                        if !literals.contains(var_name) {
                            bindings
                                .entry(var_name.clone())
                                .or_default()
                                .extend(ellipsis_matches);
                        }
                    }

                    pattern_idx += 1;
                }
                _ => {
                    // Regular pattern - must match exactly one item
                    self.match_pattern_recursive(
                        &patterns[pattern_idx],
                        std::slice::from_ref(&items[item_idx]),
                        literals,
                        bindings,
                    )?;
                    pattern_idx += 1;
                    item_idx += 1;
                }
            }
        }

        // Check if we've consumed all patterns and items appropriately
        if pattern_idx < patterns.len() {
            // We have remaining patterns - check if they can all be satisfied with zero matches
            for remaining_pattern in &patterns[pattern_idx..] {
                if let MacroPattern::Ellipsis(_) = remaining_pattern {
                    // Ellipsis can match zero items, so this is OK
                    continue;
                } else {
                    return Err(MacroError(
                        "Not enough items to match remaining patterns".to_string(),
                    ));
                }
            }
        }

        if item_idx < items.len() {
            return Err(MacroError("Too many items for pattern".to_string()));
        }

        Ok(())
    }

    /// Recursive pattern matching helper
    fn match_pattern_recursive(
        &self,
        pattern: &MacroPattern,
        args: &[Value],
        literals: &[String],
        bindings: &mut PatternBindings,
    ) -> Result<(), MacroError> {
        match pattern {
            MacroPattern::Variable(name) => {
                if literals.contains(name) {
                    // This is a literal - must match exactly
                    if args.len() != 1 {
                        return Err(MacroError(
                            "Literal pattern expects exactly one argument".to_string(),
                        ));
                    }
                    if let Value::Symbol(s) = &args[0] {
                        if s == name {
                            return Ok(());
                        }
                    }
                    Err(MacroError(format!("Expected literal {}", name)))
                } else {
                    // This is a pattern variable - bind it
                    if args.len() != 1 {
                        return Err(MacroError(
                            "Pattern variable expects exactly one argument".to_string(),
                        ));
                    }
                    bindings.insert(name.clone(), vec![args[0].clone()]);
                    Ok(())
                }
            }
            MacroPattern::List(sub_patterns) => {
                // The args should be a single list to match against the pattern
                if args.len() != 1 {
                    return Err(MacroError(
                        "List pattern expects exactly one list argument".to_string(),
                    ));
                }

                if let Value::List(list_items) = &args[0] {
                    self.match_list_pattern(sub_patterns, list_items, literals, bindings)
                } else {
                    Err(MacroError(
                        "List pattern requires a list to match against".to_string(),
                    ))
                }
            }
            MacroPattern::Ellipsis(sub_pattern) => {
                // Match zero or more instances of the sub-pattern
                let mut matched_values = Vec::new();
                let mut i = 0;

                while i < args.len() {
                    // Try to match the sub-pattern against remaining args
                    let mut sub_bindings = HashMap::new();

                    match self.match_pattern_recursive(
                        sub_pattern,
                        &args[i..i + 1],
                        literals,
                        &mut sub_bindings,
                    ) {
                        Ok(()) => {
                            // Sub-pattern matched, collect the bindings
                            for (_var, values) in sub_bindings {
                                matched_values.extend(values);
                            }
                            i += 1;
                        }
                        Err(_) => {
                            // Sub-pattern didn't match, we're done with ellipsis
                            break;
                        }
                    }
                }

                // For ellipsis patterns, we need to handle variable binding differently
                // The sub-pattern variables should bind to lists of matched values
                match sub_pattern.as_ref() {
                    MacroPattern::Variable(name) if !literals.contains(name) => {
                        bindings
                            .entry(name.clone())
                            .or_default()
                            .extend(matched_values);
                    }
                    _ => {
                        // For more complex sub-patterns, this needs more sophisticated handling
                        // For now, just collect all matched values
                    }
                }

                Ok(())
            }
            MacroPattern::Literal(lit) => {
                if args.len() != 1 {
                    return Err(MacroError(
                        "Literal pattern expects exactly one argument".to_string(),
                    ));
                }
                let arg_str = format!("{}", args[0]);
                if arg_str == *lit {
                    Ok(())
                } else {
                    Err(MacroError(format!(
                        "Expected literal {}, got {}",
                        lit, arg_str
                    )))
                }
            }
        }
    }

    /// Instantiate a template with pattern bindings
    fn instantiate_template(
        &mut self,
        template: &MacroTemplate,
        bindings: &PatternBindings,
    ) -> Result<Value, MacroError> {
        match template {
            MacroTemplate::Variable(name) => {
                if let Some(values) = bindings.get(name) {
                    if values.len() == 1 {
                        // Track emitted symbols from pattern variables
                        self.track_emitted_macro_symbols(&values[0]);
                        Ok(values[0].clone())
                    } else {
                        Err(MacroError(format!(
                            "Pattern variable {} bound to multiple values",
                            name
                        )))
                    }
                } else {
                    // Not a pattern variable - return as literal symbol and track it
                    if self.known_macro_symbols.contains(name) {
                        self.emitted_macro_symbols.insert(name.clone());
                    }
                    Ok(Value::Symbol(name.clone()))
                }
            }
            MacroTemplate::Literal(value) => {
                self.track_emitted_macro_symbols(value);
                Ok(value.clone())
            }
            MacroTemplate::List(sub_templates) => {
                let mut result = Vec::new();

                for sub_template in sub_templates {
                    match sub_template {
                        MacroTemplate::Ellipsis(ellipsis_sub_template) => {
                            // **BUG FIX:** Handle ellipsis expansion properly by splicing values
                            match ellipsis_sub_template.as_ref() {
                                MacroTemplate::Variable(name) => {
                                    // Simple variable ellipsis: splice all bound values
                                    if let Some(values) = bindings.get(name) {
                                        for value in values {
                                            self.track_emitted_macro_symbols(value);
                                            result.push(value.clone());
                                        }
                                    }
                                    // If no values bound, splice nothing (zero matches)
                                }
                                MacroTemplate::List(ellipsis_sub_templates) => {
                                    // Complex ellipsis: find max repetition length
                                    let mut max_len = 0;
                                    for template in ellipsis_sub_templates {
                                        if let MacroTemplate::Variable(var_name) = template {
                                            if let Some(values) = bindings.get(var_name) {
                                                max_len = max_len.max(values.len());
                                            }
                                        }
                                    }

                                    // Expand the sub-template for each repetition
                                    for i in 0..max_len {
                                        let mut instance_bindings = bindings.clone();
                                        // Create single-value bindings for this instance
                                        for values in instance_bindings.values_mut() {
                                            if i < values.len() {
                                                *values = vec![values[i].clone()];
                                            } else {
                                                values.clear();
                                            }
                                        }

                                        let expanded = self.instantiate_template(
                                            &MacroTemplate::List(ellipsis_sub_templates.clone()),
                                            &instance_bindings,
                                        )?;
                                        result.push(expanded);
                                    }
                                }
                                _ => {
                                    return Err(MacroError(
                                        "R7RS DEVIATION: Complex ellipsis templates not fully supported".to_string()
                                    ));
                                }
                            }
                        }
                        _ => {
                            // Non-ellipsis template: instantiate normally
                            result.push(self.instantiate_template(sub_template, bindings)?);
                        }
                    }
                }

                Ok(Value::List(result))
            }
            MacroTemplate::Ellipsis(sub_template) => {
                // Expand the sub-template for each bound value
                let mut expanded_values = Vec::new();

                // Find variables in the sub-template that have multiple bindings
                match sub_template.as_ref() {
                    MacroTemplate::Variable(name) => {
                        if let Some(values) = bindings.get(name) {
                            expanded_values.extend(values.clone());
                        }
                    }
                    MacroTemplate::List(sub_templates) => {
                        // For list templates, we need to find the maximum binding length
                        let mut max_len = 0;
                        for template in sub_templates {
                            if let MacroTemplate::Variable(var_name) = template {
                                if let Some(values) = bindings.get(var_name) {
                                    max_len = max_len.max(values.len());
                                }
                            }
                        }

                        // Expand the list template for each repetition
                        for i in 0..max_len {
                            let mut instance_bindings = bindings.clone();
                            // Modify bindings to contain single values for this instance
                            for values in instance_bindings.values_mut() {
                                if i < values.len() {
                                    *values = vec![values[i].clone()];
                                } else {
                                    values.clear();
                                }
                            }

                            let expanded = self.instantiate_template(
                                &MacroTemplate::List(sub_templates.clone()),
                                &instance_bindings,
                            )?;
                            expanded_values.push(expanded);
                        }
                    }
                    _ => {
                        // For other template types, this needs more work
                        return Err(MacroError(
                            "Complex ellipsis templates not yet supported".to_string(),
                        ));
                    }
                }

                Ok(Value::List(expanded_values))
            }
        }
    }

    /// Track symbols emitted in a value (recursive for lists)
    /// Track macro symbols emitted during template instantiation for short-circuit optimization
    fn track_emitted_macro_symbols(&mut self, value: &Value) {
        match value {
            Value::Symbol(name) => {
                if self.known_macro_symbols.contains(name) {
                    self.emitted_macro_symbols.insert(name.clone());
                }
            }
            Value::List(items) => {
                for item in items {
                    self.track_emitted_macro_symbols(item);
                }
            }
            _ => {} // Other value types don't contain symbols
        }
    }

    /// Get a macro definition from the local macro storage
    fn get_macro(&self, name: &str) -> Option<MacroDefinition> {
        self.macros.get(name).cloned()
    }

    /// Generate a unique symbol for hygiene (simple gensym for now)
    /// **R7RS DEVIATION:** Simple counter-based gensym, not proper hygienic renaming
    #[allow(dead_code)]
    fn gensym(&mut self, base: &str) -> String {
        self.gensym_counter += 1;
        format!("{}#{}", base, self.gensym_counter)
    }

    /// Check if a pattern contains nested ellipsis patterns
    /// **R7RS DEVIATION:** Helper to detect unsupported nested ellipsis
    #[allow(clippy::only_used_in_recursion)]
    fn contains_ellipsis(&self, pattern: &MacroPattern) -> bool {
        match pattern {
            MacroPattern::Ellipsis(_) => true,
            MacroPattern::List(patterns) => patterns.iter().any(|p| self.contains_ellipsis(p)),
            _ => false,
        }
    }

    /// Store a macro definition in the local macro storage
    fn store_macro(&mut self, macro_def: MacroDefinition) -> Result<(), MacroError> {
        // Track this symbol as a known macro
        self.known_macro_symbols.insert(macro_def.name.clone());
        self.macros.insert(macro_def.name.clone(), macro_def);
        Ok(())
    }

    /// Get the count of known macro symbols
    pub fn known_macro_count(&self) -> usize {
        self.known_macro_symbols.len()
    }

    /// Load standard R7RS derived expressions from macro prelude
    ///
    /// This method loads all the standard Scheme macros that are defined as
    /// "derived expressions" in R7RS specification, including:
    /// - `and`, `or` - logical operators (section 4.2.1)
    /// - `when`, `unless` - simple conditionals (section 4.2.6)
    /// - `cond`, `case` - multi-way conditionals (sections 4.2.1, 4.2.5)
    /// - `let`, `let*` - local binding forms (section 4.2.2)
    /// - `do` - iteration form (section 4.2.4)
    ///
    /// These are loaded from prelude/macros.scm at startup and are essential
    /// for a complete R7RS-compliant Scheme implementation.
    pub fn load_prelude(&mut self) -> Result<(), MacroError> {
        // Load R7RS derived expressions from external .scm file
        // This file contains all the standard macros that MUST be available
        // in any compliant Scheme implementation
        const MACRO_PRELUDE: &str = include_str!("../prelude/macros.scm");

        use crate::parser::parse_multiple;

        // Parse all define-syntax statements using the multi-statement parser
        match parse_multiple(MACRO_PRELUDE) {
            Ok(expressions) => {
                for ast in expressions {
                    if let Err(e) = self.expand_once(&ast) {
                        return Err(MacroError(format!(
                            "Failed to load prelude macro '{}': {}\nThe interpreter cannot continue with a broken prelude.", 
                            ast, e
                        )));
                    }
                }
            }
            Err(e) => {
                return Err(MacroError(format!(
                    "Failed to parse prelude macros: {}\nThe interpreter cannot continue with a broken prelude.", 
                    e
                )));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Environment;

    #[test]
    fn test_parse_simple_syntax_rules() {
        let env = Rc::new(Environment::new());
        let expander = MacroExpander::new(env);

        // (syntax-rules () [(when test body) (if test body)])
        let syntax_rules = Value::List(vec![
            Value::Symbol("syntax-rules".to_string()),
            Value::List(vec![]), // Empty literals
            Value::List(vec![
                Value::List(vec![
                    Value::Symbol("when".to_string()),
                    Value::Symbol("test".to_string()),
                    Value::Symbol("body".to_string()),
                ]),
                Value::List(vec![
                    Value::Symbol("if".to_string()),
                    Value::Symbol("test".to_string()),
                    Value::Symbol("body".to_string()),
                ]),
            ]),
        ]);

        let result = expander.parse_syntax_rules("when", &syntax_rules);
        assert!(result.is_ok());

        let macro_def = result.unwrap();
        assert_eq!(macro_def.name, "when");
        assert_eq!(macro_def.literals.len(), 0);
        assert_eq!(macro_def.rules.len(), 1);
    }

    #[test]
    fn test_pattern_parsing() {
        let env = Rc::new(Environment::new());
        let expander = MacroExpander::new(env);

        // Simple symbol pattern
        let pattern = Value::Symbol("test".to_string());
        let result = expander.parse_pattern(&pattern);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), MacroPattern::Variable("test".to_string()));

        // List pattern
        let pattern = Value::List(vec![
            Value::Symbol("when".to_string()),
            Value::Symbol("test".to_string()),
        ]);
        let result = expander.parse_pattern(&pattern);
        assert!(result.is_ok());
    }
}
