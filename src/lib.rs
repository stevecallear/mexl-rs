mod ast;
mod builtin;
mod code;
mod compiler;
mod environment;
mod lexer;
mod object;
mod parser;
mod token;
mod vm;

use crate::compiler::{Compiler};
use crate::parser::Parser;
use crate::lexer::Lexer;
use crate::vm::VM;

pub use crate::environment::Environment;
pub use crate::object::Object;
pub use crate::compiler::Program;

pub fn compile(input: &str) -> Result<Program, String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expr = parser.parse()?;

    let mut compiler = Compiler::new();
    compiler.compile(&expr)?;
    Ok(compiler.program())
}

pub fn run(program: Program, env: &Environment) -> Result<Object, String> {
    let mut vm = VM::new(program);
    vm.run(env)
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
        let prog = compile(input).unwrap();
        let result = run(prog, &env).unwrap();

        assert!((result == true.into()))
    }
}