# Macro System Refactoring Plan

## Current State
- ✅ `macro_compiler.rs` - COMPLETE AND VALIDATED
  - Parses syntax-rules forms into CompiledRule structures
  - Validates patterns/templates for R7RS compliance
  - Pre-computes metadata (variables, ellipsis depths, max depth)
  - Enforces ellipsis position, literals restrictions
  - Error detection at definition time with clear messages

## Target Architecture

### Module Organization

```
src/
  macro_compiler.rs      ✅ DONE - Definition-time compilation
  macro_matcher.rs       🔄 TODO - Runtime pattern matching
  macro_expander.rs      ⏳ TODO - Template expansion  
  macros.rs              ⏳ TODO - Integration layer (slim down)
```

### Detailed Module Responsibilities

#### `macro_compiler.rs` (ALREADY COMPLETE)
- **Input**: Raw `syntax-rules` Value from parser
- **Output**: `MacroDefinition` with `CompiledRule[]`
- **Responsibilities**:
  - Parse patterns and templates
  - Validate R7RS compliance (ellipsis position, literals, etc.)
  - Compute metadata (variable sets, ellipsis depth maps)
  - Detect depth inconsistencies
- **Status**: ✅ Complete, tested, documented

#### `macro_matcher.rs` (NEXT - START HERE)
- **Input**: `CompiledPattern` + input `Value` (the form being matched)
- **Output**: `MatchResult` containing `MatchContext` with `Binding` hierarchy
- **Key Data Structures**:
  ```rust
  pub enum Binding {
      Direct(Value),              // depth 0 - single value
      Repeated {                  // depth N - list of bindings at depth N-1
          count: usize,
          elements: Vec<Binding>,
      },
  }
  
  pub struct MatchContext {
      bindings: HashMap<String, Binding>,
  }
  
  pub enum MatchResult {
      Success(MatchContext),
      Failure(String),           // reason for non-match
  }
  ```
- **Core Functions**:
  - `match_pattern(pattern: &CompiledPattern, value: &Value, literals: &[String]) -> MatchResult`
  - `match_list_pattern(patterns: &[MacroPattern], values: &[Value], context: &mut MatchContext, literals: &[String]) -> bool`
  - `match_ellipsis_pattern(pattern: &MacroPattern, values: &[Value], context: &mut MatchContext, literals: &[String]) -> bool`
- **Key Design Principles**:
  1. Use compiled metadata (ellipsis_depth map) to know expected binding depths
  2. Build hierarchical Binding structures correctly from the start
  3. For ellipsis patterns, collect all repetitions before binding variables
  4. Each variable gets ONE binding at the correct depth (not multiple conflicting bindings)
- **Testing Strategy**:
  - Test with `debug_macro_only.rs` tool
  - Start with simple patterns: `(x)` → Direct binding
  - Then ellipsis: `(x ...)` → Repeated binding with count
  - Then nested: `((x ...) ...)` → Repeated of Repeated
  - Validate binding structure matches expected depth from compiler

#### `macro_expander.rs` (AFTER MATCHER WORKS)
- **Input**: `CompiledTemplate` + `MatchContext` with bindings
- **Output**: Expanded `Value`
- **Key Functions**:
  - `expand_template(template: &CompiledTemplate, context: &MatchContext) -> Result<Value, MacroError>`
  - `expand_list_template(templates: &[MacroTemplate], context: &MatchContext) -> Result<Vec<Value>, MacroError>`
  - `expand_ellipsis_template(template: &MacroTemplate, context: &MatchContext) -> Result<Vec<Value>, MacroError>`
- **Key Design Principles**:
  1. Use compiled metadata to know which variables need ellipsis expansion
  2. For ellipsis templates, determine repetition count from bindings
  3. Recursively extract elements at each depth level
  4. All variables in an ellipsis must have same repetition count (validated by compiler)
- **Testing Strategy**:
  - Test with known-good bindings from matcher tests
  - Verify simple substitution: `x` with `Direct(5)` → `5`
  - Verify ellipsis replication: `(x ...)` with `Repeated{count:3, [1,2,3]}` → `(1 2 3)`
  - Verify nested ellipsis expansion

#### `macros.rs` (SLIM INTEGRATION LAYER)
- **Responsibilities**:
  - `MacroExpander` struct - stores macro definitions
  - `expand_until_stable()` - iterative expansion loop
  - `handle_define_syntax()` - delegate to macro_compiler
  - Prelude loading (macros.scm, functions.scm)
  - Orchestration: compiler → matcher → expander
- **Keep**:
  - MacroExpander struct and constructor
  - expand() public API
  - expand_until_stable() loop
  - Prelude loading logic
  - Debug flag and debug_trace! macro (move to common location?)
- **Remove**:
  - All pattern/template parsing (now in macro_compiler)
  - All pattern matching logic (move to macro_matcher)
  - All template expansion logic (move to macro_expander)
  - Binding construction (move to macro_matcher)

## Action Plan

### Phase 1: Design and Implement macro_matcher.rs ⏰ CURRENT
1. ✅ Document the plan (this file)
2. Create `macro_matcher.rs` with:
   - Fresh `Binding` enum (clean implementation)
   - `MatchContext` struct
   - `MatchResult` enum
   - Core matching functions using compiled metadata
3. Add module to `lib.rs`
4. Test with `debug_macro_only.rs`:
   - Create test macros with various pattern depths
   - Print both compiled metadata AND match results
   - Verify binding structure matches compiled depths
5. Iterate until all test patterns match correctly

### Phase 2: Implement macro_expander.rs
1. Create `macro_expander.rs` with:
   - Template expansion functions
   - Ellipsis replication logic
   - Use compiled metadata for expansion control
2. Add module to `lib.rs`
3. Test with `debug_macro_only.rs`:
   - Use validated bindings from Phase 1
   - Test expansion with various template depths
   - Verify output matches expected R7RS behavior
4. Run full macro test suite

### Phase 3: Slim Down macros.rs
1. Remove all deleted code (parsing, matching, expansion internals)
2. Keep only:
   - MacroExpander struct
   - Public expand() API
   - expand_until_stable() orchestration
   - Prelude loading
3. Update to use macro_matcher and macro_expander modules
4. Test full system integration

### Phase 4: Validation
1. Run all 29 failing tests - should now pass
2. Run comprehensive R7RS test suite
3. Update TODO.md with results
4. Update documentation

## Key Insights from Bug Analysis

The current bug (variables bound to lists instead of elements) happens because:
- Pattern matching doesn't distinguish between "match this value" and "match and replicate"
- Variables get bound multiple times at different depths
- Binding construction doesn't respect the compiled depth metadata

**Solution**: Use the compiled `ellipsis_depth` map from `CompiledPattern` to know exactly what depth each variable should be bound at, then construct the Binding hierarchy correctly from the start.

## Testing Strategy

Use `debug_macro_only.rs` as our validation tool:
1. Enhance it to show both compiled metadata AND match results
2. Create focused test files for each pattern type
3. Validate binding structure matches expected depths
4. Build up complexity: simple → ellipsis → nested ellipsis

## Expected Outcomes

After refactoring:
- ✅ Clear separation of concerns (compile / match / expand)
- ✅ Each module testable in isolation
- ✅ Bug fixed: correct hierarchical binding construction
- ✅ All 29 failing tests pass
- ✅ Easier to maintain and extend
- ✅ Better error messages (can pinpoint which phase failed)
