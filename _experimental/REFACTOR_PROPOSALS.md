# Big Bang Refactor Proposals for macros.rs

## Proposal 1: Hierarchical Binding System (RECOMMENDED)

**Problems Solved:** Issues #1, #2, #10 (Critical pattern matching bug, nested ellipsis structure, single list hack)

**Core Insight:** The fundamental problem is that we're trying to represent hierarchical ellipsis nesting with a flat `HashMap<String, BindingValue>` where `BindingValue` only has two levels. This forces workarounds and special cases.

### Current System (Broken):
```rust
enum BindingValue {
    Single(Value),           // One value
    List(Vec<BindingValue>), // Multiple values, but recursion doesn't work well
}
```

### Proposed System:
```rust
#[derive(Debug, Clone, PartialEq)]
enum Binding {
    /// Direct value binding (no ellipsis)
    Direct(Value),
    
    /// Ellipsis binding with repetition count and sub-bindings
    Repeated {
        count: usize,
        elements: Vec<Binding>,  // Each repetition can contain nested Repeated
    },
}

struct MatchContext {
    bindings: HashMap<String, Binding>,
}
```

### Key Changes:

1. **Pattern Matching:**
   - When matching `((var ...) body) ...`:
     - Outer ellipsis creates `Repeated { count: 2, elements: [...] }`
     - Inner ellipsis creates nested `Repeated` for `var`
     - Each variable maintains its own repetition structure

2. **Template Expansion:**
   - Ellipsis expansion walks the `Repeated` structure naturally
   - No special cases for "single list bindings"
   - Nested ellipsis just recurses through `Repeated` hierarchy

3. **Variable Context Tracking:**
   - Each ellipsis match creates a NEW context with proper scoping
   - Variables can't bleed across pattern positions
   - Fixes the critical bug #1 automatically

### Example:
```scheme
;; Pattern: ((var ...) body) ...
;; Input: ((a b) (+ a b)) ((x y z) (* x y z))

// Bindings created:
var -> Repeated {
    count: 2,
    elements: [
        Repeated { count: 2, elements: [Direct(Symbol("a")), Direct(Symbol("b"))] },
        Repeated { count: 3, elements: [Direct(Symbol("x")), Direct(Symbol("y")), Direct(Symbol("z"))] }
    ]
}

body -> Repeated {
    count: 2,
    elements: [
        Direct(List([Symbol("+"), Symbol("a"), Symbol("b")])),
        Direct(List([Symbol("*"), Symbol("x"), Symbol("y"), Symbol("z")]))
    ]
}
```

### Implementation Plan:
1. Define new `Binding` enum (replaces `BindingValue`)
2. Rewrite `match_list_pattern_new` to build hierarchical `Binding` structures
3. Rewrite `expand_ellipsis_new` to traverse `Repeated` recursively
4. Remove all special-case single-list handling
5. Update `collect_pattern_vars` to track depth context

**Estimated Impact:** 
- Removes ~100 lines of special-case code
- Fixes 3 critical issues simultaneously
- Makes code self-documenting (structure matches R7RS semantics)
- Easier to add vector/improper list support later

---

## Proposal 2: Separate Pattern Compilation Phase

**Problems Solved:** Issues #6, #7 (Duplicate vars, literal matching)

**Core Insight:** We're validating and interpreting patterns during matching, which is inefficient and error-prone. Compile patterns once, use many times.

### Current System:
```rust
fn match_pattern_new(&self, pattern: &MacroPattern, value: &Value, ...) -> bool {
    // Pattern interpretation mixed with matching
}
```

### Proposed System:
```rust
struct CompiledPattern {
    structure: PatternNode,
    variables: HashSet<String>,      // Validated unique
    literal_positions: HashMap<usize, String>, // Pre-computed
    ellipsis_depths: HashMap<String, usize>,   // Variable -> nesting depth
}

enum PatternNode {
    Literal { value: String, is_symbolic: bool },
    Variable { name: String, depth: usize },
    Wildcard,
    List { elements: Vec<PatternNode> },
    Ellipsis { inner: Box<PatternNode>, min_matches: usize },
}

impl MacroDefinition {
    fn compile(rules: Vec<SyntaxRule>) -> Result<Self, MacroError> {
        // Validate ALL rules upfront:
        // - Check for duplicate pattern variables
        // - Verify ellipsis variables exist in templates
        // - Pre-compute literal positions
        // - Calculate ellipsis nesting depths
    }
}
```

### Benefits:
1. **Validation Once:** Check duplicate variables at macro definition time
2. **Faster Matching:** Pre-computed structure, no repeated checks
3. **Better Errors:** Can show which rule has issues during compilation
4. **Literal Matching:** Pre-mark which positions need symbolic comparison
5. **Depth Tracking:** Know ellipsis nesting level for each variable

### Example:
```scheme
(define-syntax bad (syntax-rules () ((bad x x) 'oops)))
;; Error at define-syntax time: "Pattern variable 'x' appears multiple times"

(define-syntax test (syntax-rules (special) ((test special x) 'matched)))
;; Compiled: literal_positions = {1: "special"}, variables = {"x"}
```

---

## Proposal 3: Context-Aware Template Expansion

**Problems Solved:** Issues #5, #11 (Underscore, quote handling)

**Core Insight:** Template expansion should know its syntactic context (quoted, unquoted, parameter position, etc.) to handle special forms correctly.

### Current System:
```rust
fn expand_template_with_quote(&self, template: &MacroTemplate, 
                               context: &MatchContext, 
                               in_quote: bool) -> Result<Value, MacroError>
```

Boolean flag is insufficient for all contexts.

### Proposed System:
```rust
#[derive(Debug, Clone, Copy)]
enum ExpansionContext {
    Normal,              // Regular template expansion
    Quoted,              // Inside (quote ...)
    LambdaParams,        // Lambda parameter list (can validate symbols)
    Definition,          // Inside (define ...) - special handling
}

fn expand_template_contextual(
    &self, 
    template: &MacroTemplate,
    bindings: &MatchContext,
    context: ExpansionContext
) -> Result<Value, MacroError> {
    match (template, context) {
        (MacroTemplate::Variable("_"), _) => {
            // Always literal underscore, never bound
            Ok(Value::Symbol("_".to_string()))
        }
        (MacroTemplate::Variable(name), ExpansionContext::LambdaParams) => {
            // Validate result is a symbol
            let val = expand_variable(name, bindings)?;
            if !matches!(val, Value::Symbol(_)) {
                return Err(MacroError("Lambda parameter must be symbol"));
            }
            Ok(val)
        }
        (MacroTemplate::List(templates), ExpansionContext::Normal) => {
            // Detect special forms and switch context
            match templates.get(0) {
                Some(MacroTemplate::Variable("quote")) => {
                    expand_list(templates, bindings, ExpansionContext::Quoted)
                }
                Some(MacroTemplate::Variable("lambda")) if templates.len() >= 3 => {
                    // Expand params with LambdaParams context
                    let params = expand_template_contextual(
                        &templates[1], bindings, ExpansionContext::LambdaParams)?;
                    // Expand body with Normal context
                    let body = expand_templates(&templates[2..], bindings, ExpansionContext::Normal)?;
                    Ok(Value::List(vec![Value::Symbol("lambda".to_string()), params, body]))
                }
                _ => expand_list(templates, bindings, context)
            }
        }
        // ... other cases
    }
}
```

### Benefits:
1. **Better Validation:** Catch lambda parameter errors during expansion
2. **Clearer Code:** Context explicit in function signature
3. **Special Form Handling:** Natural place for `quote`, `lambda`, etc.
4. **Underscore Fixed:** Single obvious place to handle it

---

## Proposal 4: Proper Improper List Support (Future-Proof)

**Problems Solved:** Issue #3 (Improper lists)

**Core Insight:** `Value::List` can't represent improper lists. Need fundamental change to value representation.

### Current System:
```rust
pub enum Value {
    List(Vec<Value>),  // Only proper lists
    // ...
}
```

### Proposed System:
```rust
pub enum Value {
    Pair(Box<Value>, Box<Value>),  // Cons cell - can build any list structure
    Nil,                            // Empty list
    // ... other types
}

// Helper constructors:
impl Value {
    fn proper_list(items: Vec<Value>) -> Value {
        items.into_iter().rev().fold(Value::Nil, |acc, item| {
            Value::Pair(Box::new(item), Box::new(acc))
        })
    }
    
    fn improper_list(items: Vec<Value>, tail: Value) -> Value {
        items.into_iter().rev().fold(tail, |acc, item| {
            Value::Pair(Box::new(item), Box::new(acc))
        })
    }
    
    fn to_vec(&self) -> Option<Vec<Value>> {
        // Convert proper list to Vec, return None for improper
    }
}
```

### Pattern Support:
```rust
enum MacroPattern {
    // ... existing variants
    ImproperList {
        elements: Vec<MacroPattern>,
        tail: Box<MacroPattern>,  // Matches rest after dot
    }
}
```

### Impact:
- **Large change** affecting entire codebase (parser, compiler, VM)
- But necessary for full R7RS conformance
- Unifies list representation with Scheme semantics
- Natural support for variadic patterns

---

## Recommendation: Phased Approach

### Phase 1: Hierarchical Binding System (Proposal 1)
**Priority:** CRITICAL  
**Effort:** 1-2 days  
**Impact:** Fixes the 29 failing tests immediately

**Why First:**
- Solves the most critical bugs
- Other refactors depend on stable pattern matching
- Self-contained (doesn't require other changes)

### Phase 2: Pattern Compilation (Proposal 2)
**Priority:** HIGH  
**Effort:** 1 day  
**Impact:** Cleaner code, better errors, faster execution

**Why Second:**
- Builds on stable matching from Phase 1
- Easier to implement with working pattern matcher
- Validates R7RS compliance systematically

### Phase 3: Context-Aware Expansion (Proposal 3)
**Priority:** MEDIUM  
**Effort:** 0.5 days  
**Impact:** Fixes special form handling, better validation

**Why Third:**
- Natural after stable expansion pipeline
- Smaller, incremental improvement
- Can be done piecemeal

### Phase 4: Improper Lists (Proposal 4)
**Priority:** LOW (but required for full R7RS)  
**Effort:** 3-4 days (touches entire codebase)  
**Impact:** Full R7RS list semantics

**Why Last:**
- Most disruptive change
- Requires stable foundation
- Can be deferred if time-constrained

---

## Code Deletion Estimates

After all refactors:

- **Remove:** ~150 lines of special-case handling
- **Remove:** ~50 lines of dead/redundant code
- **Add:** ~200 lines of cleaner, self-documenting code
- **Net:** Similar LOC, but 3x better maintainability

**Files to delete after refactors:**
- `debug_simple_first.rs` - no longer needed
- `debug_minimal_ellipsis.rs` - no longer needed
- `debug_ellipsis_only.rs` - no longer needed
- Various other debug binaries

---

## Testing Strategy

For each phase:

1. **Create test suite FIRST** based on R7RS spec
2. **Implement refactor** to pass tests
3. **Remove old code** only after new code passes
4. **Update documentation** to reflect new architecture

---

*Would you like me to implement Phase 1 (Hierarchical Binding System)? It's the most impactful and self-contained refactor.*
