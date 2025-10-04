# Pure CPS Architecture Design Trade-offs

This document outlines the key architectural decisions for implementing pure CPS (Continuation-Passing Style) in the rulesxp Scheme interpreter.

## Context

Analysis shows pure CPS implementation is superior to dual-mode compatibility:
- **2x more inlining opportunities** (80% vs 40%)
- **1.7x faster function calls** (15 vs 25 cycles)  
- **3x less memory per call** (32 vs 96 bytes)
- **60% smaller VM implementation** (800 vs 2000 LOC)
- **Eliminates stack overflow risk** entirely

Decision: **Pure CPS with no legacy direct-call compatibility**

## Architectural Decision Points

### 1. Continuation Representation

**Question:** How should we represent continuations in the runtime?

#### Option A: Closure-based Continuations
```rust
// Continuations as Value::Procedure with captured environment
enum Value {
    Procedure {
        params: Vec<String>,
        body: Vec<Value>,
        env: Rc<Environment>,
        is_continuation: bool,  // Mark as continuation
    },
    // ... other types
}
```

**Pros:**
- **Uniform representation** - continuations are just procedures
- **Simple implementation** - reuse existing procedure infrastructure
- **Natural garbage collection** - follows same lifetime as procedures
- **Easy debugging** - continuations visible as lambda expressions

**Cons:**
- **Heap allocation overhead** - every continuation creates heap object
- **GC pressure** - many short-lived continuation objects
- **Call overhead** - continuation invocation goes through full procedure call

#### Option B: Stack-based Continuations
```rust
struct CPSRuntime {
    value_stack: Vec<Value>,
    continuation_stack: Vec<ContinuationFrame>,
}

struct ContinuationFrame {
    instruction_pointer: usize,
    environment: Rc<Environment>,
    stack_base: usize,
}
```

**Pros:**
- **Fast invocation** - direct jump to instruction pointer
- **Stack allocation** - no heap allocation for continuations
- **Compact representation** - just IP + environment reference
- **Better cache locality** - continuations stored contiguously

**Cons:**
- **Still has stack growth** - continuation stack can overflow
- **Complex stack management** - need to handle stack unwinding
- **Limited first-class support** - harder to pass continuations as values
- **Debugging complexity** - continuations not visible as values

#### Option C: Bytecode-based Continuations
```rust
enum Continuation {
    Bytecode {
        instructions: Vec<Opcode>,
        environment: Rc<Environment>,
    },
    Closure {
        procedure: Value,
    },
}
```

**Pros:**
- **Compact representation** - continuations as instruction sequences
- **Compiler optimization** - can inline/optimize continuation bytecode
- **Fast execution** - direct bytecode execution
- **Hybrid approach** - can use closures when needed

**Cons:**
- **Implementation complexity** - need separate continuation compiler
- **Limited composability** - harder to manipulate continuation bytecode
- **Debugging difficulty** - bytecode not human-readable
- **Code size** - need both continuation and procedure compilation

#### Option D: Hybrid Approach
```rust
enum Continuation {
    Inlined(InlinedContinuation),    // For simple cases
    Closure(Value),                   // For complex cases
    Native(fn(&[Value]) -> Value),   // For builtin continuations
}
```

**Pros:**
- **Best performance** - optimal representation for each case
- **Gradual optimization** - start simple, optimize hot paths
- **Flexibility** - different strategies for different situations
- **Future-proof** - can add new continuation types

**Cons:**
- **Implementation complexity** - multiple code paths to maintain
- **Analysis overhead** - compiler must choose representation
- **Testing complexity** - need to test all continuation types
- **Potential bugs** - more code paths = more potential issues

**Recommendation:** Start with **Option A (Closure-based)** for initial implementation due to simplicity and uniformity. Can evolve to Option D as optimization needs arise.

### 2. Builtin Function Integration

**Question:** How should Rust builtin functions integrate with CPS calling convention?

#### Option A: CPS Wrappers
```rust
// Wrap existing Rust functions
fn add_builtin(args: &[Value]) -> Result<Value, RuntimeError> {
    // args = [x, y, continuation]
    let x = get_number(&args[0])?;
    let y = get_number(&args[1])?;
    let continuation = &args[2];
    let result = Value::Number(x + y);
    
    // Call continuation with result
    call_continuation(continuation, &[result])
}
```

**Scheme usage:**
```scheme
(+ 1 2 (lambda (result) (display result)))
```

**Pros:**
- **Minimal changes** to existing builtin functions
- **Uniform calling convention** - all functions take continuation
- **Easy implementation** - just wrap existing logic
- **Gradual migration** - can convert builtins one by one

**Cons:**
- **Call overhead** - extra function call for every builtin
- **Continuation allocation** - heap allocation for simple operations
- **Less optimization** - wrapper prevents inlining
- **Verbose syntax** - requires explicit continuations in user code

#### Option B: Native CPS Builtins
```rust
// Rewrite builtins to be natively CPS-aware
fn add_cps(x: i64, y: i64, k: Continuation) -> CPSResult {
    let result = x + y;
    invoke_continuation(k, Value::Number(result))
}
```

**Integration:**
```rust
// Compiler generates optimized calls
match builtin_name {
    "+" => compile_add_cps(args, continuation),
    "*" => compile_multiply_cps(args, continuation),
    // ...
}
```

**Pros:**
- **Maximum performance** - no wrapper overhead
- **Compiler optimization** - can inline builtin operations
- **Clean semantics** - builtins are true CPS functions
- **Type safety** - compile-time arity checking

**Cons:**
- **More implementation work** - rewrite all builtins
- **Breaks uniformity** - special cases in compiler
- **Testing complexity** - need CPS-specific tests
- **Migration effort** - all builtins need conversion

#### Option C: Compiler Magic
```rust
// Special-case arithmetic for performance
match expression {
    ["+", x, y] if in_arithmetic_context() => {
        // Compile to direct bytecode, bypass CPS
        compile_direct_add(x, y, continuation)
    }
    _ => compile_cps_call(expression, continuation)
}
```

**Pros:**
- **Best performance** - no CPS overhead for arithmetic
- **Transparent optimization** - user doesn't see complexity
- **Gradual rollout** - optimize hot paths individually
- **Maintains compatibility** - with existing Scheme semantics

**Cons:**
- **Breaks CPS uniformity** - not all calls are CPS
- **Complex compiler** - special cases everywhere
- **Hidden complexity** - optimization not visible to user
- **Debugging issues** - mixed execution models

**Recommendation:** **Option A (CPS Wrappers)** initially for uniformity and simplicity. Can optimize to Option B for performance-critical operations once the base system is working.

### 3. Program Entry Point

**Question:** How should programs start execution in pure CPS?

#### Option A: Identity Continuation
```scheme
;; User writes:
(+ 1 2)

;; Gets compiled to:
(+ 1 2 identity)

;; Where identity is:
(define identity (lambda (x) x))
```

**Pros:**
- **Mathematical purity** - identity is the natural terminal continuation
- **Composability** - easy to chain programs together
- **Flexibility** - user can provide different initial continuations
- **Debugging clarity** - obvious what the continuation does

**Cons:**
- **Extra call overhead** - always pays for identity function call
- **User confusion** - why does my program return via identity?
- **Limited utility** - identity doesn't do much

#### Option B: Display Continuation
```scheme
;; User writes:
(+ 1 2)

;; Gets compiled to:
(+ 1 2 display-and-exit)

;; Where display-and-exit is:
(define display-and-exit (lambda (x) (display x) (newline) (exit)))
```

**Pros:**
- **User-friendly** - automatically displays results
- **REPL-like behavior** - matches user expectations
- **No extra work** - user doesn't need to handle result
- **Clean output** - consistent formatting

**Cons:**
- **Less flexible** - forces display behavior
- **Harder composition** - can't easily chain programs
- **Side effects** - always performs I/O
- **Exit behavior** - terminates program

#### Option C: Configurable Continuation
```rust
// Allow user to specify initial continuation
struct CPSRuntime {
    initial_continuation: Continuation,
}

impl CPSRuntime {
    fn new_repl() -> Self {
        Self { initial_continuation: display_continuation() }
    }
    
    fn new_script() -> Self {
        Self { initial_continuation: identity_continuation() }
    }
}
```

**Pros:**
- **Maximum flexibility** - appropriate for different use cases
- **User control** - can specify desired behavior
- **Mode-aware** - REPL vs script vs library different behaviors
- **Future-proof** - can add new continuation types

**Cons:**
- **Configuration complexity** - more options to understand
- **API surface** - more ways for things to go wrong
- **Implementation complexity** - need multiple initial continuations

**Recommendation:** **Option B (Display Continuation)** for simplicity and user-friendliness, with Option C as a future enhancement for library/embedding use cases.

### 4. Error Handling in CPS

**Question:** How should errors propagate in CPS world?

#### Option A: Exception Continuations
```scheme
;; Every function takes success and error continuations
(/ 1 0 
   (lambda (result) (display result))      ; success continuation
   (lambda (error) (display-error error))) ; error continuation
```

**Pros:**
- **Explicit error handling** - errors visible in type system
- **Composable** - can chain error handlers
- **No exceptions** - pure functional error handling
- **Fine-grained control** - different error strategies per call

**Cons:**
- **Verbose syntax** - every call needs two continuations
- **User burden** - must always provide error handlers
- **Performance overhead** - extra continuation for every call
- **Complexity** - harder to understand for beginners

#### Option B: Result Continuations
```scheme
;; Continuations receive Result<Value, Error>
(/ 1 0 (lambda (result)
  (if (error? result)
    (handle-error (error-value result))
    (continue-with (success-value result)))))
```

**Pros:**
- **Single continuation** - simpler calling convention
- **Result type** - familiar from Rust/functional languages
- **Uniform handling** - same pattern for all error handling
- **Optional checking** - can ignore errors if desired

**Cons:**
- **Runtime overhead** - every result needs error checking
- **Easy to ignore** - errors can be accidentally ignored
- **Boxing overhead** - results need to be wrapped
- **Scheme unfamiliarity** - Result types not native to Scheme

#### Option C: Global Error Handler
```rust
// Errors escape CPS and use Rust's Result system
fn add_cps(x: i64, y: i64, k: Continuation) -> Result<(), RuntimeError> {
    let result = x.checked_add(y)
        .ok_or_else(|| RuntimeError::overflow("addition overflow"))?;
    invoke_continuation(k, Value::Number(result))
}
```

**Pros:**
- **Simple implementation** - reuse existing error handling
- **Performance** - no CPS overhead for error cases
- **Rust integration** - natural error propagation
- **Clean happy path** - no error handling clutter in common case

**Cons:**
- **Not pure CPS** - errors break CPS invariants
- **Limited control** - can't customize error handling per call
- **Exception model** - not functional error handling
- **Debugging complexity** - stack unwinding through CPS calls

#### Option D: Scheme Exceptions with dynamic-wind
```scheme
;; Implement proper Scheme exception system
(guard (condition 
        ((error? condition) (display-error condition)))
  (/ 1 0))
```

**Pros:**
- **R7RS compliant** - follows Scheme standards
- **Full-featured** - supports dynamic-wind, exception hierarchy
- **Familiar** - matches other Scheme implementations
- **Powerful** - can implement complex error handling patterns

**Cons:**
- **Complex implementation** - dynamic-wind is non-trivial in CPS
- **Performance overhead** - exception machinery has costs
- **Large scope** - significant implementation effort
- **CPS complexity** - exceptions + CPS = complex interactions

**Recommendation:** **Option C (Global Error Handler)** initially for simplicity, with Option D as a future enhancement once the core CPS system is stable.

### 5. Macro System Integration

**Question:** How do macros work in pure CPS world?

#### Option A: CPS-Aware Macros
```scheme
;; Macros must explicitly handle continuations
(define-syntax when-cps
  (syntax-rules ()
    ((when-cps test body k)
     (if test (begin body k) (k (unspecified))))))

;; Usage:
(when-cps #t (display "hello") my-continuation)
```

**Pros:**
- **Explicit control** - macro author controls CPS transformation
- **Maximum flexibility** - can optimize for specific CPS patterns
- **No magic** - clear how continuations flow through macro
- **Predictable** - user knows what CPS code will be generated

**Cons:**
- **User burden** - macro authors must understand CPS
- **Breaks compatibility** - existing macros don't work
- **Verbose** - every macro invocation needs continuation
- **Learning curve** - CPS macro programming is complex

#### Option B: Automatic CPS Transform
```scheme
;; Macros expand to direct style, compiler transforms to CPS
(define-syntax when
  (syntax-rules ()
    ((when test body) (if test body))))

;; Usage (compiler automatically adds continuation):
(when #t (display "hello"))  ; becomes (when-internal #t (display "hello") k)
```

**Pros:**
- **Compatibility** - existing macros work unchanged
- **User-friendly** - no need to understand CPS for macros
- **Automatic** - compiler handles CPS transformation
- **Gradual migration** - can port macros incrementally

**Cons:**
- **Complex compiler** - must CPS-transform arbitrary macro expansions
- **Less control** - can't optimize for specific CPS patterns
- **Hidden transformations** - not obvious what CPS code is generated
- **Potential issues** - some macros might not transform well

#### Option C: CPS Macro Primitives
```scheme
;; Provide special CPS-aware macro forms
(define-cps-syntax when
  (cps-rules ()
    ((when test body k)
     (if-cps test (begin-cps body k) k))))

;; Where if-cps, begin-cps are CPS-aware primitives
```

**Pros:**
- **Best of both worlds** - CPS-aware but with helper primitives
- **Easier than raw CPS** - primitives handle common patterns
- **Optimizable** - can optimize primitive implementations
- **Extensible** - can add new CPS primitives as needed

**Cons:**
- **New syntax** - need to learn CPS macro system
- **Implementation complexity** - need CPS primitive library
- **Migration effort** - existing macros need rewriting
- **Limited adoption** - new, unfamiliar approach

**Recommendation:** **Option B (Automatic CPS Transform)** for compatibility and ease of use, with Option C as a future enhancement for performance-critical macros.

### 6. Performance vs Simplicity Priority

**Question:** What's the priority for the initial implementation?

#### Option A: Maximum Simplicity
- Uniform closure-based continuations (no optimizations)
- CPS wrappers for all builtins
- Automatic CPS transformation of macros
- Global error handling
- Display continuation for program entry

**Timeline:** 2-4 weeks for basic implementation
**Performance:** 3-5x slowdown vs current implementation
**Complexity:** Low - reuse existing infrastructure

#### Option B: Balanced Approach
- Closure-based continuations with basic inlining
- Mix of CPS wrappers and native CPS for common operations
- Automatic CPS transformation with optimization hints
- Result-based error handling for better performance
- Configurable entry continuations

**Timeline:** 4-8 weeks for full implementation
**Performance:** 1.5-2x slowdown vs current implementation
**Complexity:** Medium - selective optimizations

#### Option C: Maximum Performance
- Hybrid continuation representation with full optimization
- Native CPS implementation for all operations
- CPS-aware macro system with optimization
- Scheme exception system with dynamic-wind
- Full compiler optimization pipeline

**Timeline:** 12-20 weeks for complete implementation
**Performance:** Potentially faster than current implementation
**Complexity:** High - comprehensive rewrite

## Recommended Architecture

Based on the trade-off analysis, I recommend starting with:

1. **Continuation Representation:** Option A (Closure-based) - simple and uniform
2. **Builtin Integration:** Option A (CPS Wrappers) - minimal changes required
3. **Program Entry:** Option B (Display Continuation) - user-friendly
4. **Error Handling:** Option C (Global Error Handler) - simple implementation
5. **Macro System:** Option B (Automatic CPS Transform) - compatibility
6. **Implementation Priority:** Option A (Maximum Simplicity) initially

This provides a working pure CPS implementation quickly, with clear optimization paths for the future.

## Implementation Phases

### Phase 1: Core CPS Infrastructure (2-3 weeks)
- Implement closure-based continuations
- Convert basic builtins (+, -, *, /, =, <, etc.)
- Simple CPS compiler transformation
- Display continuation for REPL

### Phase 2: Language Feature Support (2-3 weeks)
- CPS transformation for all language constructs
- Macro system integration
- Function definition and application
- Basic error handling

### Phase 3: Optimization (4-6 weeks)
- Continuation inlining for simple cases
- Native CPS for performance-critical builtins
- Dead continuation elimination
- Tail call optimization verification

### Phase 4: Advanced Features (future)
- Hybrid continuation representation
- Full Scheme exception system
- CPS-aware macro primitives
- Advanced compiler optimizations

This phased approach allows for incremental development while maintaining a working system at each stage.