$newContent = @'
// Macro expansion system - implements R7RS syntax-rules macros
use crate::value::{Environment, Value};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub const STANDARD_DERIVED_EXPRESSIONS: &[&str] = &[
    "and", "or", "when", "unless", "cond", "case", "let", "let*", "do",
];

#[derive(Debug, Clone)]
pub struct MacroError(pub String);

impl std::fmt::Display for MacroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Macro error: {}", self.0)
    }
}

impl std::error::Error for MacroError {}

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
}

#[derive(Debug, Clone, PartialEq)]
pub enum BindingValue {
    Single(Value),
    List(Vec<BindingValue>),
}

pub type PatternBindings = HashMap<String, BindingValue>;

pub struct MacroExpander {
    macros: HashMap<String, MacroDefinition>,
    known_macro_symbols: HashSet<String>,
    expansion_dirty: bool,
}

impl MacroExpander {
    pub fn new(_environment: Rc<Environment>) -> Self {
        MacroExpander {
            macros: HashMap::new(),
            known_macro_symbols: HashSet::new(),
            expansion_dirty: false,
        }
    }

    pub fn expand(&mut self, ast: &Value) -> Result<Value, MacroError> {
        self.expand_until_stable(ast)
    }

    fn expand_until_stable(&mut self, ast: &Value) -> Result<Value, MacroError> {
        use std::borrow::Cow;
        const MAX_EXPANSIONS: usize = 100;
        let mut current = Cow::Borrowed(ast);
        let mut expansion_count = 0;

        loop {
            self.expansion_dirty = false;
            let expanded = self.expand_once(&current)?;

            if !self.expansion_dirty {
                return Ok(current.into_owned());
            }

            current = Cow::Owned(expanded);
            expansion_count += 1;

            if expansion_count > MAX_EXPANSIONS {
                return Err(MacroError(format!(
                    "Expression expansion exceeded {} iterations. This suggests infinite macro expansion.",
                    MAX_EXPANSIONS
                )));
            }
        }
    }

    fn expand_once(&mut self, ast: &Value) -> Result<Value, MacroError> {
        match ast {
            Value::List(items) if !items.is_empty() => {
                if let Value::Symbol(name) = &items[0] {
                    if name == "define-syntax" {
                        return self.handle_define_syntax(items);
                    }
                    if let Some(macro_def) = self.get_macro(name) {
                        self.expansion_dirty = true;
                        return self.expand_macro_rule(&macro_def, items);
                    }
                }

                let expanded_items: Result<Vec<_>, _> =
                    items.iter().map(|item| self.expand_once(item)).collect();

                let final_items: Vec<_> = expanded_items?
                    .into_iter()
                    .filter(|v| !matches!(v, Value::Unspecified))
                    .collect();

                if final_items != *items {
                    self.expansion_dirty = true;
                }
                Ok(Value::List(final_items))
            }
            _ => Ok(ast.clone()),
        }
    }

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
        let macro_def = self.parse_syntax_rules(&name, &items[2])?;
        self.store_macro(macro_def)?;
        Ok(Value::Unspecified)
    }

    fn parse_syntax_rules(
        &self,
        name: &str,
        syntax_rules: &Value,
    ) -> Result<MacroDefinition, MacroError> {
        match syntax_rules {
            Value::List(items)
                if items.len() >= 2
                    && matches!(&items[0], Value::Symbol(s) if s == "syntax-rules") =>
            {
                let literals = self.parse_literals(&items[1])?;
                let rules_values = &items[2..];

                let rules = rules_values
                    .iter()
                    .map(|rule_item| self.parse_rule(rule_item, &literals))
                    .collect::<Result<Vec<_>, _>>()?;

                if rules.is_empty() {
                    return Err(MacroError(format!(
                        "syntax-rules for '{}' must have at least one rule.",
                        name
                    )));
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

    fn parse_literals(&self, literals_val: &Value) -> Result<Vec<String>, MacroError> {
        match literals_val {
            Value::List(items) => items
                .iter()
                .map(|item| match item {
                    Value::Symbol(s) => Ok(s.clone()),
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

    fn parse_rule(&self, rule: &Value, literals: &[String]) -> Result<SyntaxRule, MacroError> {
        match rule {
            Value::List(items) if items.len() == 2 => {
                let pattern = self.parse_pattern(&items[0], literals)?;
                let template = self.parse_template(&items[1])?;
                Ok(SyntaxRule { pattern, template })
            }
            _ => Err(MacroError(
                "Rule must be a list of pattern and template".to_string(),
            )),
        }
    }

    fn parse_pattern(
        &self,
        pattern: &Value,
        literals: &[String],
    ) -> Result<MacroPattern, MacroError> {
        match pattern {
            Value::Symbol(s) => {
                if literals.contains(s) {
                    Ok(MacroPattern::Literal(s.clone()))
                } else if s == "_" {
                    Ok(MacroPattern::Wildcard)
                } else {
                    Ok(MacroPattern::Variable(s.clone()))
                }
            }
            Value::List(items) => {
                let mut patterns = Vec::new();
                let mut it = items.iter().peekable();
                while let Some(item) = it.next() {
                    let sub_pattern = self.parse_pattern(item, literals)?;
                    if let Some(&Value::Symbol(s)) = it.peek() {
                        if s == "..." {
                            it.next(); // consume ellipsis
                            patterns.push(MacroPattern::Ellipsis(Box::new(sub_pattern)));
                            continue;
                        }
                    }
                    patterns.push(sub_pattern);
                }
                Ok(MacroPattern::List(patterns))
            }
            _ => Ok(MacroPattern::Literal(pattern.to_string())),
        }
    }

    fn parse_template(&self, template: &Value) -> Result<MacroTemplate, MacroError> {
        match template {
            Value::Symbol(s) => Ok(MacroTemplate::Variable(s.clone())),
            Value::List(items) => {
                let mut templates = Vec::new();
                let mut it = items.iter().peekable();
                while let Some(item) = it.next() {
                    let sub_template = self.parse_template(item)?;
                    if let Some(&Value::Symbol(s)) = it.peek() {
                        if s == "..." {
                            it.next(); // consume ellipsis
                            templates.push(MacroTemplate::Ellipsis(Box::new(sub_template)));
                            continue;
                        }
                    }
                    templates.push(sub_template);
                }
                Ok(MacroTemplate::List(templates))
            }
            _ => Ok(MacroTemplate::Literal(template.clone())),
        }
    }

    fn expand_macro_rule(
        &mut self,
        macro_def: &MacroDefinition,
        full_form: &[Value],
    ) -> Result<Value, MacroError> {
        for rule in &macro_def.rules {
            let mut bindings = PatternBindings::new();
            if self.match_pattern(
                &rule.pattern,
                &Value::List(full_form.to_vec()),
                &mut bindings,
            ) {
                return self.expand_template(&rule.template, &bindings, 0, 0);
            }
        }
        Err(MacroError(format!(
            "No matching rule for macro {}",
            macro_def.name
        )))
    }

    fn match_pattern(
        &self,
        pattern: &MacroPattern,
        value: &Value,
        bindings: &mut PatternBindings,
    ) -> bool {
        self.match_pattern_recursive(pattern, value, bindings, 0)
            .is_ok()
    }

    fn match_pattern_recursive(
        &self,
        pattern: &MacroPattern,
        value: &Value,
        bindings: &mut PatternBindings,
        level: usize,
    ) -> Result<(), ()> {
        match (pattern, value) {
            (MacroPattern::Literal(lit), Value::Symbol(s)) if lit == s => Ok(()),
            (MacroPattern::Literal(lit), other) if lit == &other.to_string() => Ok(()),
            (MacroPattern::Variable(name), _) => {
                self.add_binding(bindings, name.clone(), value.clone(), level);
                Ok(())
            }
            (MacroPattern::Wildcard, _) => Ok(()),
            (MacroPattern::List(sub_patterns), Value::List(sub_values)) => {
                self.match_list_pattern(sub_patterns, sub_values, bindings, level)
            }
            _ => Err(()),
        }
    }

    fn add_binding(&self, bindings: &mut PatternBindings, name: String, value: Value, level: usize) {
        if level == 0 {
            bindings.insert(name, BindingValue::Single(value));
            return;
        }

        let entry = bindings.entry(name).or_insert_with(|| {
            let mut root = BindingValue::List(Vec::new());
            let mut current = &mut root;
            for _ in 1..level {
                let new_list = BindingValue::List(Vec::new());
                if let BindingValue::List(items) = current {
                    items.push(new_list);
                    current = items.last_mut().unwrap();
                }
            }
            root
        });

        let mut current = entry;
        for _ in 0..level {
            if let BindingValue::List(items) = current {
                if items.is_empty() {
                    items.push(BindingValue::List(Vec::new()));
                }
                current = items.last_mut().unwrap();
            }
        }

        if let BindingValue::List(items) = current {
            items.push(BindingValue::Single(value));
        }
    }

    fn match_list_pattern<'a>(
        &self,
        patterns: &[MacroPattern],
        values: &'a [Value],
        bindings: &mut PatternBindings,
        level: usize,
    ) -> Result<(), ()> {
        let mut value_cursor = 0;
        let mut pattern_cursor = 0;

        while pattern_cursor < patterns.len() {
            let pattern = &patterns[pattern_cursor];

            if let MacroPattern::Ellipsis(sub_pattern) = pattern {
                let next_pattern_slice = &patterns[pattern_cursor + 1..];
                let mut ellipsis_end = values.len();

                if !next_pattern_slice.is_empty() {
                    for i in value_cursor..=values.len() {
                        let mut temp_bindings = bindings.clone();
                        if self.match_list_pattern(next_pattern_slice, &values[i..], &mut temp_bindings, level).is_ok() {
                            ellipsis_end = i;
                            break;
                        }
                    }
                }
                
                let ellipsis_values = &values[value_cursor..ellipsis_end];
                for val in ellipsis_values {
                    if self.match_pattern_recursive(sub_pattern, val, bindings, level + 1).is_err() {
                        return Err(());
                    }
                }

                let vars_in_ellipsis = self.get_vars_in_pattern(sub_pattern);
                for var in &vars_in_ellipsis {
                    if !bindings.contains_key(var) {
                         self.add_binding(bindings, var.clone(), Value::List(vec![]), level + 1);
                    }
                }

                value_cursor = ellipsis_end;
                pattern_cursor += 1; // Move to the pattern after the ellipsis
            } else {
                if value_cursor >= values.len() {
                    return Err(());
                }
                if self.match_pattern_recursive(pattern, &values[value_cursor], bindings, level).is_err() {
                    return Err(());
                }
                value_cursor += 1;
                pattern_cursor += 1;
            }
        }

        if value_cursor == values.len() {
            Ok(())
        } else {
            Err(())
        }
    }

    fn expand_template(
        &self,
        template: &MacroTemplate,
        bindings: &PatternBindings,
        ellipsis_level: usize,
        quote_level: usize,
    ) -> Result<Value, MacroError> {
        match template {
            MacroTemplate::Literal(val) => Ok(val.clone()),
            MacroTemplate::Variable(name) => {
                if quote_level > 0 {
                    return Ok(Value::Symbol(name.clone()));
                }
                match get_binding_at_level(bindings, name, ellipsis_level) {
                    Some(BindingValue::Single(v)) => Ok(v.clone()),
                    Some(bv @ BindingValue::List(_)) => Ok(binding_value_to_value(bv)),
                    None => Ok(Value::Symbol(name.clone())),
                }
            }
            MacroTemplate::List(templates) => {
                if let Some(Value::Symbol(s)) = templates.get(0).and_then(|t| match t {
                    MacroTemplate::Variable(name) => Some(Value::Symbol(name.clone())),
                    MacroTemplate::Literal(v) => Some(v.clone()),
                    _ => None,
                }) {
                    if s == "quote" {
                        let mut items = vec![Value::Symbol("quote".to_string())];
                        for t in &templates[1..] {
                            items.push(self.expand_template(t, bindings, ellipsis_level, quote_level + 1)?);
                        }
                        return Ok(Value::List(items));
                    }
                }

                let mut items = Vec::new();
                for t in templates {
                    items.push(self.expand_template(t, bindings, ellipsis_level, quote_level)?);
                }
                Ok(Value::List(items))
            }
            MacroTemplate::Ellipsis(sub_template) => {
                if quote_level > 0 {
                    let mut items = self.expand_template(sub_template, bindings, ellipsis_level, quote_level)?;
                    if let Value::List(ref mut list) = items {
                        list.push(Value::Symbol("...".to_string()));
                    }
                    return Ok(items);
                }

                let vars = self.get_vars_in_template(sub_template);
                let num_repetitions = self.get_repetition_count(&vars, bindings, ellipsis_level)?;

                let mut expanded_items = Vec::new();
                for i in 0..num_repetitions {
                    let local_bindings = self.create_local_bindings(&vars, bindings, ellipsis_level, i)?;
                    let expanded = self.expand_template(sub_template, &local_bindings, ellipsis_level + 1, quote_level)?;
                    expanded_items.push(expanded);
                }
                Ok(Value::List(expanded_items))
            }
        }
    }

    fn get_repetition_count(
        &self,
        vars: &HashSet<String>,
        bindings: &PatternBindings,
        ellipsis_level: usize,
    ) -> Result<usize, MacroError> {
        let mut num_repetitions = None;
        for var in vars {
            if let Some(binding) = get_binding_at_level(bindings, var, ellipsis_level) {
                if let BindingValue::List(items) = binding {
                    let len = items.len();
                    if let Some(n) = num_repetitions {
                        if len != n {
                            return Err(MacroError(format!(
                                "Inconsistent repetition counts for ellipsis variables: expected {}, found {} for '{}'",
                                n, len, var
                            )));
                        }
                    } else {
                        num_repetitions = Some(len);
                    }
                }
            }
        }
        Ok(num_repetitions.unwrap_or(0))
    }

    fn create_local_bindings(
        &self,
        vars: &HashSet<String>,
        bindings: &PatternBindings,
        ellipsis_level: usize,
        index: usize,
    ) -> Result<PatternBindings, MacroError> {
        let mut local_bindings = bindings.clone();
        for var in vars {
            if let Some(binding) = get_binding_at_level(bindings, var, ellipsis_level) {
                if let BindingValue::List(items) = binding {
                    if let Some(val) = items.get(index) {
                        local_bindings.insert(var.clone(), val.clone());
                    }
                }
            }
        }
        Ok(local_bindings)
    }

    fn get_vars_in_pattern(&self, pattern: &MacroPattern) -> HashSet<String> {
        let mut vars = HashSet::new();
        match pattern {
            MacroPattern::Variable(name) => {
                vars.insert(name.clone());
            }
            MacroPattern::List(patterns) => {
                for p in patterns {
                    vars.extend(self.get_vars_in_pattern(p));
                }
            }
            MacroPattern::Ellipsis(sub_pattern) => {
                vars.extend(self.get_vars_in_pattern(sub_pattern));
            }
            _ => {}
        }
        vars
    }

    fn get_vars_in_template(&self, template: &MacroTemplate) -> HashSet<String> {
        let mut vars = HashSet::new();
        match template {
            MacroTemplate::Variable(name) => {
                vars.insert(name.clone());
            }
            MacroTemplate::List(templates) => {
                for t in templates {
                    vars.extend(self.get_vars_in_template(t));
                }
            }
            MacroTemplate::Ellipsis(sub_template) => {
                vars.extend(self.get_vars_in_template(sub_template));
            }
            _ => {}
        }
        vars
    }

    fn store_macro(&mut self, macro_def: MacroDefinition) -> Result<(), MacroError> {
        let name = macro_def.name.clone();
        self.macros.insert(name.clone(), macro_def);
        self.known_macro_symbols.insert(name);
        Ok(())
    }

    fn get_macro(&self, name: &str) -> Option<MacroDefinition> {
        self.macros.get(name).cloned()
    }

    pub fn load_prelude(&mut self) -> Result<(), MacroError> {
        let prelude_path = "prelude/macros.scm";
        let prelude_content = std::fs::read_to_string(prelude_path)
            .map_err(|e| MacroError(format!("Failed to read prelude file: {}", e)))?;

        let expressions = crate::parser::parse_multiple(&prelude_content)
            .map_err(|e| MacroError(format!("Failed to parse prelude file: {}", e)))?;

        for expr in expressions {
            self.expand_once(&expr)?;
        }

        Ok(())
    }
}

fn get_binding_at_level<'a>(
    bindings: &'a PatternBindings,
    name: &str,
    level: usize,
) -> Option<&'a BindingValue> {
    let mut current = bindings.get(name)?;
    for _ in 0..level {
        if let BindingValue::List(items) = current {
            // This assumption is tricky. When descending levels for a specific variable,
            // we assume the structure is uniform. We take the first item's structure
            // as representative. This might fail for ragged lists.
            if let Some(first) = items.get(0) {
                current = first;
            } else {
                return None; // Level is deeper than the binding structure.
            }
        } else {
            return None; // Not a list, cannot descend further.
        }
    }
    Some(current)
}

fn binding_value_to_value(bv: &BindingValue) -> Value {
    match bv {
        BindingValue::Single(v) => v.clone(),
        BindingValue::List(lst) => Value::List(lst.iter().map(binding_value_to_value).collect()),
    }
}
'@
Set-Content -Path "c:\src\sexpr\rulesxp\_experimental\src\macros.rs" -Value $newContent -NoNewline
Write-Host "c:\src\sexpr\rulesxp\_experimental\src\macros.rs has been overwritten with the new implementation."

