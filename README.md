# mexl
[![build](https://github.com/stevecallear/mexl-rs/actions/workflows/test.yml/badge.svg)](https://github.com/stevecallear/mexl-rs/actions)

`mexl` is a simple expression language. Originally written in [Go](https://github.com/stevecallear/mexl), it has been ported as a learning exercise and to provide a stronger foundation for future WASM/WASI targets.

It is intended to offer a simple syntax for expressions, but with high performance and a type system designed to reduce complexity when handling unknown inputs.

## Getting Started

```rust
use mexl::{Environment};

let program = mexl::compile("email ew \"@email.com\" or \"beta\" in roles")?;

let mut env = Environment::default();
env.set("email", "user@email.com".into());
env.set("roles", vec!["admin".into(), "beta".into()].into());

let result = mexl::run(program, &env)?;

assert!(result == true.into());
```

## Types

`mexl` is generally statically typed, but ensures null coalescing and numeric type coercion where appropriate. Unlike the Go implementation, type casting is fully implemented with an enhanced truthiness model to ensure predictable outputs.

| Type | Rust Type |
|------|-----------|
| integer | i64 |
| float | f64 |
| string | String |
| boolean | bool |
| array | Vec |
| map | HashMap |
| null | null |

### Null Coalescing

Nulls are coalesced by default. The following expression would evaluate to null:

```rust
let out = mexl::eval("x.y.z", &env)?; // x is not defined, out is null
```

### Type Coercion

Numeric expressions containing float and integer values will result in a float value:

```rust
let out = mexl::eval("1 + 1.1", &env)?; // out is 2.1
```

Integer division truncates the result:

```rust
let out = mexl::eval("3 / 2", &env)?; // out is 1
```

To force float division, cast explicitly:

```rust
let out = mexl::eval("3 as float / 2", &env)?; // out is 1.5
```

If a binary expression contains a null value it will be coerced to the default value of the appropriate type:

```rust
let out = mexl::eval("null + 1", &env)?; // out is 1
```

Null coercion is intended to avoid constant null checks or runtime errors when the input values and their types cannot be guaranteed. For example:

```rust
let out = mexl::eval(r#"lower(x.y) ew "abc""#, &env)?; // x is not defined, out is false
```

Null checks can be performed using the `eq` or `ne` operators if required:

```rust
let out = mexl::eval("x.y eq null", &env)?; // x is not defined, out is true
```

## Operations

The following operations are supported:

| Operator | Symbol | Description |
|----------|--------|-------------|
| eq | == | equal |
| ne | != | not equal |
| lt | < | less than |
| gt | > | greater than |
| le | <= | less than or equal |
| ge | >= | greater than or equal |
| sw | | starts with |
| ew | | ends with |
| in | | in an array or string |
| and | && | and |
| or | \|\| | or |
| not | ! | not |

## Functions

The following built-in functions are available:

| Function | Description |
|----------|-------------|
| len | the length of the string, array or map |
| lower | the lowercase representation of the string |
| upper | the uppercase representation of the string |

### Custom Functions

Custom functions can be specified for the `Environment` using `define_function`.

Generally a function should:
- **Guarantee a return type** (e.g., `len()` always returns an integer)
- **Handle null gracefully** by returning the default value of the return type
- **Raise an error for invalid non-null input** (user must cast if needed)

Example:

```rust
let program = mexl::compile("reverse(word) ew \"eh\"")?;
    
let mut env = Environment::default();
env.set("word", "hello".into());
env.define_function("reverse", |args| {
    // args length checking omitted for brevity
    match args[0] {
        Object::Null => Ok(Object::default_string()), // explictly handle null
        Object::String(ref s) => {
            let reversed: String = s.chars().rev().collect();
            Ok(reversed.into())
        }
        _ => Err(mexl::MexlError::RuntimeError(
            "Invalid argument type".into(),
        )),
    }
});

let result = mexl::run(program, &env)?;
assert!(result == true.into());
```

## Architecture

The implementation follows a classic bytecode compilation pipeline:

```
Input String → [Lexer → Parser → Compiler] → Program → [VM] → Result
```

1. **Lexer** - Tokenizes input into a stream of tokens
2. **Parser** - Builds an abstract syntax tree (AST) using Pratt parsing
3. **Compiler** - Translates AST to bytecode instructions with constant/identifier tables
4. **VM** - Stack-based virtual machine that executes bytecode with an environment context

## Design Rules
* Any infix operation attempts to unify operands before execution. Null values adopt the default of the other type. Mismatched integer/float expressions unify to float/float. All other type mismatches remain unchanged.
* Integer to float conversion must be specified by the user. Integer algebraic operations produce integer output (e.g., `3 / 2 = 1`). Cast to float for float division (e.g., `(3 as float) / 2 = 1.5`). Casting the result does not affect algebra (e.g., `(3 / 2) as float = 1.0`).
* Null values coalesce in all scenarios—no runtime error occurs due to absent values.
* A builtin has a guaranteed return type (e.g., integer for `len`). It handles null input gracefully but is not responsible for casting. Invalid non-null input raises a runtime error (the user can cast beforehand if needed).

## Development

Build and test the library:

```bash
cargo build       # Compile the library
cargo test        # Run all tests
cargo clippy      # Run linter checks
cargo fmt         # Format code
```

See [src/lib.rs](src/lib.rs) and the copilot-instructions.md for more details on the architecture and development workflows.

## Future

- [ ] Error if multiple top-level expressions (currently terminates early)
- [ ] Index operator for arrays/strings (e.g., `arr[0]`, `str[1]`)
- [ ] Array members via dot notation (e.g., `arr.0 == arr[0]`)
- [ ] Slice operator for arrays/strings (e.g., `arr[1..5]`, `str[..3]`)