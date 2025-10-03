// Macro expansion system - implements R7RS syntax-rules macros
use crate::value::{Value, Environment};
use std::rc::Rc;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct MacroError(pub String);

impl std::fmt::Display for MacroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Macro error: {}", self.0)
    }
}

impl std::error::Error for MacroError {}

/// A pattern in a syntax-rules macro
#[derive(Debug, Clone, PartialEq)]
pub enum MacroPattern {
    /// Literal identifier that must match exactly
    Literal(String),
    /// Pattern variable that binds to any expression
    Variable(String),
    /// List pattern containing sub-patterns
    List(Vec<MacroPattern>),
    /// Ellipsis pattern for zero or more repetitions
    Ellipsis(Box<MacroPattern>),
}

/// A template for macro expansion
#[derive(Debug, Clone, PartialEq)]
pub enum MacroTemplate {
    /// Literal value to insert
    Literal(Value),
    /// Pattern variable to substitute
    Variable(String),
    /// List template containing sub-templates
    List(Vec<MacroTemplate>),
    /// Ellipsis template for expanding repeated patterns
    Ellipsis(Box<MacroTemplate>),
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

/// Multi-pass macro expander with smart pass detection
pub struct MacroExpander {
    environment: Rc<Environment>,
    macros: HashMap<String, MacroDefinition>,
    gensym_counter: usize,
    /// Symbols that are known to be macros
    known_macro_symbols: std::collections::HashSet<String>,
    /// Symbols emitted during the current expansion pass
    emitted_symbols: std::collections::HashSet<String>,
}

impl MacroExpander {
    pub fn new(environment: Rc<Environment>) -> Self {
        MacroExpander {
            environment,
            macros: HashMap::new(),
            gensym_counter: 0,
            known_macro_symbols: HashSet::new(),
            emitted_symbols: HashSet::new(),
        }
    }

    /// Expand macros in the given AST with smart multi-pass support
    pub fn expand(&mut self, ast: &Value) -> Result<Value, MacroError> {
        let mut current = ast.clone();
        let mut pass_count = 0;
        const MAX_EXPANSION_PASSES: usize = 100; // Safety limit to prevent infinite loops
        
        loop {
            // Clear emitted symbols for this pass
            self.emitted_symbols.clear();
            
            let expanded = self.expand_single_pass(&current)?;
            
            if expanded == current {
                // No changes in this pass - expansion complete
                return Ok(expanded);
            }
            
            current = expanded;
            pass_count += 1;
            
            // Safety check to prevent infinite expansion
            if pass_count >= MAX_EXPANSION_PASSES {
                return Err(MacroError(format!(
                    "Macro expansion exceeded safety limit of {} passes", 
                    MAX_EXPANSION_PASSES
                )));
            }
            
            // Check if we need another pass by seeing if any emitted symbols are macros
            let needs_another_pass = self.emitted_symbols.iter()
                .any(|symbol| self.known_macro_symbols.contains(symbol));
            
            // If no emitted symbols are macros, we're done
            if !needs_another_pass {
                return Ok(current);
            }
        }
    }

    /// Single pass through the AST expanding any macros found
    fn expand_single_pass(&mut self, ast: &Value) -> Result<Value, MacroError> {
        match ast {
            Value::List(items) if !items.is_empty() => {
                // Check if this is a define-syntax form
                if let Value::Symbol(name) = &items[0] {
                    if name == "define-syntax" {
                        return self.handle_define_syntax(items);
                    }
                    
                    // Block local macro definitions for now
                    if name == "let-syntax" || name == "letrec-syntax" {
                        return Err(MacroError(format!(
                            "{} is not supported yet - only top-level define-syntax is currently implemented",
                            name
                        )));
                    }
                    
                    // Check if this is a macro invocation
                    if let Some(macro_def) = self.get_macro(name) {
                        return self.expand_macro(&macro_def, &items[1..]);
                    }
                }
                
                // Not a macro - recursively expand all elements
                let expanded: Result<Vec<_>, _> = items.iter()
                    .map(|item| self.expand_single_pass(item))
                    .collect();
                Ok(Value::List(expanded?))
            }
            // Other value types don't contain macros
            _ => Ok(ast.clone())
        }
    }

    /// Handle define-syntax forms
    fn handle_define_syntax(&mut self, items: &[Value]) -> Result<Value, MacroError> {
        if items.len() != 3 {
            return Err(MacroError("define-syntax requires exactly 2 arguments".to_string()));
        }

        let name = match &items[1] {
            Value::Symbol(s) => s.clone(),
            _ => return Err(MacroError("define-syntax name must be a symbol".to_string())),
        };

        let syntax_rules = &items[2];
        let macro_def = self.parse_syntax_rules(&name, syntax_rules)?;
        
        // Store the macro in the environment
        self.store_macro(macro_def)?;
        
        // Return unspecified value (define-syntax doesn't produce a value)
        Ok(Value::Unspecified)
    }

    /// Parse a syntax-rules form into a MacroDefinition
    fn parse_syntax_rules(&self, name: &str, syntax_rules: &Value) -> Result<MacroDefinition, MacroError> {
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
                    return Err(MacroError("syntax-rules must have at least one rule".to_string()));
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
            _ => Err(MacroError("Rule must be a list of pattern and template".to_string())),
        }
    }

    /// Parse a pattern from a rule
    fn parse_pattern(&self, pattern: &Value) -> Result<MacroPattern, MacroError> {
        match pattern {
            Value::Symbol(s) => {
                // Check if this is a literal or a pattern variable
                // For now, we'll implement this simply - anything in the literals list is literal
                Ok(MacroPattern::Variable(s.clone()))
            }
            Value::List(items) => {
                let mut patterns = Vec::new();
                let mut i = 0;
                while i < items.len() {
                    if i + 1 < items.len() {
                        if let Value::Symbol(s) = &items[i + 1] {
                            if s == "..." {
                                // This is an ellipsis pattern
                                let sub_pattern = self.parse_pattern(&items[i])?;
                                patterns.push(MacroPattern::Ellipsis(Box::new(sub_pattern)));
                                i += 2; // Skip the ... symbol
                                continue;
                            }
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
    fn parse_template(&self, template: &Value) -> Result<MacroTemplate, MacroError> {
        match template {
            Value::Symbol(s) => {
                Ok(MacroTemplate::Variable(s.clone()))
            }
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
    fn expand_macro(&mut self, macro_def: &MacroDefinition, args: &[Value]) -> Result<Value, MacroError> {
        // Reconstruct the full macro call for pattern matching
        let mut full_call = vec![Value::Symbol(macro_def.name.clone())];
        full_call.extend_from_slice(args);
        
        // Convert to a single List value for pattern matching
        let call_as_list = Value::List(full_call);
        
        // Try each rule in order until one matches
        for rule in macro_def.rules.iter() {
            if let Ok(bindings) = self.match_pattern(&rule.pattern, std::slice::from_ref(&call_as_list), &macro_def.literals) {
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
                        return Err(MacroError("Literal pattern expects exactly one argument".to_string()));
                    }
                    if let Value::Symbol(s) = &args[0] {
                        if s == name {
                            return Ok(());
                        }
                    }
                    return Err(MacroError(format!("Expected literal {}", name)));
                } else {
                    // This is a pattern variable - bind it
                    if args.len() != 1 {
                        return Err(MacroError("Pattern variable expects exactly one argument".to_string()));
                    }
                    bindings.insert(name.clone(), vec![args[0].clone()]);
                    Ok(())
                }
            }
            MacroPattern::List(sub_patterns) => {
                // The args should be a single list to match against the pattern
                if args.len() != 1 {
                    return Err(MacroError("List pattern expects exactly one list argument".to_string()));
                }
                
                if let Value::List(list_items) = &args[0] {
                    // TODO: Handle ellipsis patterns properly
                    if sub_patterns.len() != list_items.len() {
                        return Err(MacroError(format!(
                            "Pattern length mismatch: expected {}, got {}",
                            sub_patterns.len(),
                            list_items.len()
                        )));
                    }
                    
                    for (sub_pattern, item) in sub_patterns.iter().zip(list_items.iter()) {
                        self.match_pattern_recursive(sub_pattern, std::slice::from_ref(item), literals, bindings)?;
                    }
                    Ok(())
                } else {
                    Err(MacroError("List pattern requires a list to match against".to_string()))
                }
            }
            MacroPattern::Ellipsis(_) => {
                // TODO: Implement ellipsis pattern matching
                Err(MacroError("Ellipsis patterns not yet implemented".to_string()))
            }
            MacroPattern::Literal(lit) => {
                if args.len() != 1 {
                    return Err(MacroError("Literal pattern expects exactly one argument".to_string()));
                }
                let arg_str = format!("{}", args[0]);
                if arg_str == *lit {
                    Ok(())
                } else {
                    Err(MacroError(format!("Expected literal {}, got {}", lit, arg_str)))
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
                        self.track_emitted_symbols(&values[0]);
                        Ok(values[0].clone())
                    } else {
                        Err(MacroError(format!("Pattern variable {} bound to multiple values", name)))
                    }
                } else {
                    // Not a pattern variable - return as literal symbol and track it
                    self.emitted_symbols.insert(name.clone());
                    Ok(Value::Symbol(name.clone()))
                }
            }
            MacroTemplate::Literal(value) => {
                // Track emitted symbols in literal values
                self.track_emitted_symbols(value);
                Ok(value.clone())
            }
            MacroTemplate::List(sub_templates) => {
                let mut result = Vec::new();
                for sub_template in sub_templates {
                    result.push(self.instantiate_template(sub_template, bindings)?);
                }
                Ok(Value::List(result))
            }
            MacroTemplate::Ellipsis(_) => {
                // TODO: Implement ellipsis template instantiation
                Err(MacroError("Ellipsis templates not yet implemented".to_string()))
            }
        }
    }
    
    /// Track symbols emitted in a value (recursive for lists)
    fn track_emitted_symbols(&mut self, value: &Value) {
        match value {
            Value::Symbol(name) => {
                self.emitted_symbols.insert(name.clone());
            }
            Value::List(items) => {
                for item in items {
                    self.track_emitted_symbols(item);
                }
            }
            _ => {} // Other value types don't contain symbols
        }
    }

    /// Generate a unique symbol for hygiene (simple gensym for now)
    fn gensym(&mut self, base: &str) -> String {
        self.gensym_counter += 1;
        format!("{}#{}", base, self.gensym_counter)
    }

    /// Get a macro definition from the local macro storage
    fn get_macro(&self, name: &str) -> Option<MacroDefinition> {
        self.macros.get(name).cloned()
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

    /// Get the count of emitted symbols
    pub fn emitted_symbol_count(&self) -> usize {
        self.emitted_symbols.len()
    }

    /// Get the emitted symbols (for debugging)
    pub fn emitted_symbols(&self) -> &std::collections::HashSet<String> {
        &self.emitted_symbols
    }

    /// Load standard macros from the prelude
    pub fn load_prelude(&mut self) -> Result<(), MacroError> {
        // Load macro definitions from external .scm file
        const MACRO_PRELUDE: &str = include_str!("../prelude/macros.scm");
        
        use crate::parser::parse_multiple;
        
        // Parse all define-syntax statements using the multi-statement parser
        match parse_multiple(MACRO_PRELUDE) {
            Ok(expressions) => {
                for ast in expressions {
                    if let Err(e) = self.expand_single_pass(&ast) {
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