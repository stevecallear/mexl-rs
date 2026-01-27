# AI Coding Agent Instructions for mexl-rs

## Project Overview
**mexl** is a Rust-based expression language interpreter. It evaluates simple expressions (e.g., `2 gt 1 and str ew "c"`) with support for operators, functions, type casting, and null coalescing. The architecture follows a classic interpreter pattern: Lexer → Parser → Compiler → VM.

## Architecture & Data Flow

### Core Pipeline (src/lib.rs)
```
Input String → compile() → Program → run() → Object (result)
               ↓                        ↓
          [Lexer → Parser → Compiler]  [VM]
```

1. **Lexer** (`src/lexer.rs`): Tokenizes input string into tokens
2. **Parser** (`src/parser.rs`): Builds an AST using Pratt parser with precedence levels
3. **Compiler** (`src/compiler.rs`): Translates AST to bytecode instructions with constants/identifiers tables
4. **VM** (`src/vm.rs`): Stack-based virtual machine that executes bytecode with an environment context

### Key Components

**AST (src/ast.rs)**: Expression enum represents all syntax nodes (Identifier, IntegerLiteral, Infix, Call, Cast, etc.)

**Objects (src/object.rs)**: Runtime values are wrapped in the Object enum (Integer, Float, String, Boolean, Array, Map, Function, Null). Type conversions via `cast_to_*` functions.

**Environment (src/environment.rs)**: HashMap storing variable bindings passed to VM at runtime.

**Builtins (src/builtin.rs)**: Native functions (len, lower, upper) called from compiled code. Each builtin guarantees a return type and handles null input; invalid non-null types error.

**Bytecode (src/code.rs)**: Opcodes (OpConstant, OpAdd, OpCall, etc.) and instruction encoding/decoding for VM execution.

## Critical Design Patterns

### Null Coalescing
Null values are **not errors**—they coalesce to the default of the other operand type:
- `null + 5` → `5` (null adopts integer)
- `null + "text"` → `"text"` (null adopts string)
- `null` alone → `Object::Null`

See [object.rs](src/object.rs#L35-L49) default helper methods.

### Type Unification
Infix operations attempt to unify operands before execution. Integer/float mismatch unifies to float; other mismatched types remain unchanged (no error). See [vm.rs](src/vm.rs#L150-L200) `execute_binary_operation`.

### Integer Division Semantics
Integer division **truncates** (e.g., `3 / 2 = 1`). Explicit float cast enforces float division: `(3 as float) / 2 = 1.5`. A cast of the result does not affect algebra: `(3 / 2) as float = 1.0`.

### Builtin Function Design
- **Guaranteed return type**: `len()` always returns integer
- **Null handling**: Return default type (e.g., `len(null) = 0`)
- **Invalid non-null input**: Raises error (user must cast if needed)
- Example: [builtin.rs](src/builtin.rs#L1-L20) `len` and `lower` implementations

## Developer Workflows

### Build & Test
```bash
cargo build        # Compile the library
cargo test         # Run tests (test_compile_and_run in lib.rs)
```

### Adding a Builtin Function
1. Define handler in [builtin.rs](src/builtin.rs) with signature `fn(Vec<Object>) -> Result<Object, String>`
2. Register in [compiler.rs](src/compiler.rs) `compile_call` case to emit OpCall opcode with function index
3. Register in [vm.rs](src/vm.rs) `execute_call` to map index to handler function

### Adding an Operator
1. Add token to [token.rs](src/token.rs) (e.g., TokenType::CustomOp)
2. Add precedence in [parser.rs](src/parser.rs) `get_precedence`
3. Add infix/prefix parsing in [parser.rs](src/parser.rs) `parse_infix_expression`
4. Add opcode to [code.rs](src/code.rs) (e.g., OpCustom)
5. Add compilation in [compiler.rs](src/compiler.rs) `compile_infix_expression`
6. Add VM execution in [vm.rs](src/vm.rs) `execute_binary_operation`

### Operator Precedence (Lowest to Highest)
Defined in [parser.rs](src/parser.rs#L5-L16): Or → And → Equals → Comparison → Sum → Product → Prefix → Cast → Call → Index

## Key File Reference
- **Entry point**: [src/lib.rs](src/lib.rs) — `compile()` and `run()` functions
- **Test fixtures**: [src/tests/fixtures.rs](src/tests/fixtures.rs) — reusable test data
- **Type system**: [src/object.rs](src/object.rs) — Object enum and cast functions
- **Bytecode format**: [src/code.rs](src/code.rs) — Opcode enum and instruction helpers
- **Error handling**: Uses `Result<T, String>` throughout; errors bubble up from compile/run phases

## Known Limitations & TODOs
- [ ] Error if multiple top-level expressions (currently terminates early)
- [ ] Index operator for arrays/strings (`arr[0]`, `str[1]`)
- [ ] Array members via dot notation (e.g., `arr.0 == arr[0]`)
- [ ] Slice operator for arrays/strings (e.g., `arr[1..5]`, `str[..3]`)
