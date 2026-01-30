mod ast;
mod builtin;
mod code;
mod compiler;
mod environment;
mod error;
mod lexer;
mod object;
mod parser;
mod token;
mod vm;

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;

pub use crate::compiler::Program;
pub use crate::environment::Environment;
pub use crate::error::MexlError;
pub use crate::object::{Object, unify_operands};

/// Compiles the given input string into a Program.
pub fn compile(input: &str) -> Result<Program, MexlError> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expr = parser.parse()?;

    let mut compiler = Compiler::new();
    compiler.compile(&expr)?;
    Ok(compiler.program())
}

/// Runs the given Program using the provided Environment.
pub fn run(program: &Program, env: &Environment) -> Result<Object, MexlError> {
    let mut vm = VM::new(program);
    vm.run(env)
}

/// Evaluates the given input string in the context of the provided Environment.
/// This is a convenience function that compiles and runs the input.
/// In general, separate compilation and execution should be preferred.
pub fn eval(input: &str, env: &Environment) -> Result<Object, MexlError> {
    let program = compile(input)?;
    run(&program, env)
}

#[cfg(test)]
pub mod tests {
    pub mod fixtures;

    use super::*;

    #[test]
    fn test_compile_and_run() {
        let mut env = Environment::default();
        env.set("str", "abc".into());

        let input = "2 gt 1 and str ew \"c\" and 0.5 in [\"a\", 0.5, true]";
        let program = compile(input).unwrap();
        let result = run(&program, &env).unwrap();

        assert!((result == true.into()))
    }

    #[test]
    fn test_eval() {
        let mut env = Environment::default();
        env.set("str", "abc".into());
        let result = eval(
            "2 gt 1 and str ew \"c\" and 0.5 in [\"a\", 0.5, true]",
            &env,
        )
        .unwrap();
        assert!((result == true.into()));
    }
}
