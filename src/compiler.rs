use crate::{
    Object,
    ast::{CastExpression, Expression, InfixExpression, PrefixExpression},
    code::{self, Instructions, Opcode},
    error::MexlError,
    token::TokenType,
};
use std::collections::HashMap;

/// Compiler struct that compiles AST nodes into bytecode instructions.
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    identifiers: Vec<String>,
    identifier_index: HashMap<String, usize>,
}

/// Program struct representing the compiled bytecode program.
#[derive(Clone)]
pub struct Program {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
    pub identifiers: Vec<String>,
}

impl Compiler {
    /// Creates a new Compiler instance.
    pub fn new() -> Self {
        Self {
            instructions: Instructions::default(),
            constants: Vec::new(),
            identifiers: Vec::new(),
            identifier_index: HashMap::new(),
        }
    }

    /// Finalizes and returns the compiled Program.
    pub fn program(self) -> Program {
        Program {
            instructions: self.instructions,
            constants: self.constants,
            identifiers: self.identifiers,
        }
    }

    /// Compiles an expression node into bytecode.
    pub fn compile(&mut self, node: &Expression) -> Result<(), MexlError> {
        self.compile_expression(node)?;
        self.emit(Opcode::Pop, &[]);
        Ok(())
    }

    /// Compiles an expression recursively.
    fn compile_expression(&mut self, expr: &Expression) -> Result<(), MexlError> {
        match expr {
            Expression::Infix(e) => {
                self.compile_infix_expression(e)?;
            }
            Expression::Prefix(e) => {
                self.compile_prefix_expression(e)?;
            }
            Expression::IntegerLiteral(v) => {
                let idx = self.add_constant(Object::Integer(*v));
                self.emit(Opcode::Constant, &[idx]);
            }
            Expression::FloatLiteral(v) => {
                let idx = self.add_constant(Object::Float(*v));
                self.emit(Opcode::Constant, &[idx]);
            }
            Expression::StringLiteral(v) => {
                let idx = self.add_constant(Object::String(v.clone()));
                self.emit(Opcode::Constant, &[idx]);
            }
            Expression::ArrayLiteral(v) => {
                self.compile_expressions(v)?;
                self.emit(Opcode::Array, &[v.len()]);
            }
            Expression::Boolean(v) => {
                let op = match v {
                    true => Opcode::True,
                    false => Opcode::False,
                };
                self.emit(op, &[]);
            }
            Expression::Null => {
                self.emit(Opcode::Null, &[]);
            }
            Expression::Identifier(v) => {
                let idx = self.add_identifier(v.value.clone());
                self.emit(Opcode::Global, &[idx]);
            }
            Expression::Call(e) => {
                self.compile_expression(&e.function)?;
                self.compile_expressions(&e.arguments)?;
                self.emit(Opcode::Call, &[e.arguments.len()]);
            }
            Expression::Index(e) => {
                self.compile_expression(&e.left)?;
                self.compile_expression(&e.index)?;
                self.emit(Opcode::Index, &[]);
            }
            Expression::Member(e) => {
                self.compile_expression(&e.left)?;
                let idx = self.add_identifier(e.member.value.clone());
                self.emit(Opcode::Member, &[idx]);
            }
            Expression::Cast(e) => {
                self.compile_cast_expression(e)?;
            }
        };
        Ok(())
    }

    /// Compiles a list of expressions.
    fn compile_expressions(&mut self, exprs: &[Expression]) -> Result<(), MexlError> {
        for expr in exprs {
            self.compile_expression(expr)?;
        }
        Ok(())
    }

    /// Emits a jump instruction with a placeholder for the target.
    fn emit_jump_placeholder(&mut self, op: Opcode) -> usize {
        let pos = self.instructions.len();
        self.emit(op, &[9999]);
        pos
    }

    /// Patches a jump instruction at the given offset to point to the target.
    fn patch(&mut self, offset: usize, target: usize) -> Result<(), MexlError> {
        let operand_pos = offset + 1;
        if operand_pos + 2 > self.instructions.len() {
            return Err(MexlError::CompileError("undefined jump target".into()));
        }

        let bytes = (target as u16).to_be_bytes();
        self.instructions[operand_pos] = bytes[0];
        self.instructions[operand_pos + 1] = bytes[1];

        Ok(())
    }

    /// Compiles logical infix expressions (and/or).
    fn compile_logical_op(
        &mut self,
        expr: &InfixExpression,
        jump_op: Opcode,
    ) -> Result<(), MexlError> {
        self.compile_expression(&expr.left)?;

        let jump_pos = self.emit_jump_placeholder(jump_op);

        self.emit(Opcode::Pop, &[]);

        self.compile_expression(&expr.right)?;

        let after_right_pos = self.instructions.len();
        self.patch(jump_pos, after_right_pos)
    }

    /// Compiles an infix expression node.
    fn compile_infix_expression(&mut self, expr: &InfixExpression) -> Result<(), MexlError> {
        match expr.token.token_type {
            TokenType::And => {
                return self.compile_logical_op(expr, Opcode::JumpNotTruthy);
            }
            TokenType::Or => {
                return self.compile_logical_op(expr, Opcode::JumpTruthy);
            }
            _ => {} // continue
        }

        self.compile_expression(&expr.left)?;
        self.compile_expression(&expr.right)?;
        let op = match expr.token.token_type {
            TokenType::Plus => Opcode::Add,
            TokenType::Minus => Opcode::Subtract,
            TokenType::Asterisk => Opcode::Multiply,
            TokenType::Slash => Opcode::Divide,
            TokenType::Equal => Opcode::Equal,
            TokenType::NotEqual => Opcode::NotEqual,
            TokenType::LessThan => Opcode::Less,
            TokenType::LessThanEqual => Opcode::LessOrEqual,
            TokenType::GreaterThan => Opcode::Greater,
            TokenType::GreaterThanEqual => Opcode::GreaterOrEqual,
            TokenType::StartsWith => Opcode::StartsWith,
            TokenType::EndsWith => Opcode::EndsWith,
            TokenType::In => Opcode::In,
            _ => {
                return Err(MexlError::CompileError(format!(
                    "unknown infix operator: {}",
                    expr.operator
                )));
            }
        };
        self.emit(op, &[]);
        Ok(())
    }

    /// Compiles a prefix expression node.
    fn compile_prefix_expression(&mut self, expr: &PrefixExpression) -> Result<(), MexlError> {
        self.compile_expression(&expr.right)?;
        match expr.token.token_type {
            TokenType::Bang => self.emit(Opcode::Not, &[]),
            TokenType::Minus => self.emit(Opcode::Minus, &[]),
            _ => {
                return Err(MexlError::CompileError(format!(
                    "unknown prefix operator: {}",
                    expr.operator
                )));
            }
        };
        Ok(())
    }

    /// Compiles a cast expression node.
    fn compile_cast_expression(&mut self, expr: &CastExpression) -> Result<(), MexlError> {
        self.compile_expression(&expr.left)?;
        let type_code = match expr.target_type.value.as_str() {
            "int" => code::CAST_INT,
            "float" => code::CAST_FLOAT,
            "string" => code::CAST_STRING,
            "bool" => code::CAST_BOOL,
            other => {
                return Err(MexlError::CompileError(format!(
                    "unknown cast target: {}",
                    other
                )));
            }
        };
        self.emit(Opcode::Cast, &[type_code as usize]);
        Ok(())
    }

    /// Adds a constant to the constants pool and returns its index.
    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    /// Adds an identifier to the identifiers list and returns its index.
    fn add_identifier(&mut self, ident: String) -> usize {
        if let Some(&idx) = self.identifier_index.get(&ident) {
            idx
        } else {
            let idx = self.identifiers.len();
            self.identifiers.push(ident.clone());
            self.identifier_index.insert(ident, idx);
            idx
        }
    }

    /// Emits a bytecode instruction.
    fn emit(&mut self, op: Opcode, operands: &[usize]) -> usize {
        let instruction = code::make(op, operands);
        let pos = self.instructions.len();
        self.instructions.extend_from_slice(&instruction);
        pos
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use crate::{code::make, lexer::Lexer, parser::Parser};

    use super::*;

    struct CompilerTestCase {
        input: &'static str,
        expected_constants: Vec<Object>,
        expected_identifiers: Vec<&'static str>,
        expected_instructions: Vec<Instructions>,
    }

    impl CompilerTestCase {
        fn assert(&self, actual: &Program) {
            let expected_instructions: Vec<u8> = self
                .expected_instructions
                .iter()
                .flat_map(|x| x.deref().clone())
                .collect();
            assert_eq!(
                *actual.instructions, expected_instructions,
                "instructions do not match"
            );
            assert_eq!(
                actual.constants, self.expected_constants,
                "constants do not match"
            );
            assert_eq!(
                actual.identifiers, self.expected_identifiers,
                "identifiers do not match"
            );
        }
    }

    #[test]
    fn test_prefix_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "-1",
                expected_constants: vec![1.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Minus, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "!true",
                expected_constants: Vec::new(),
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::True, &[]),
                    make(Opcode::Not, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "not true",
                expected_constants: Vec::new(),
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::True, &[]),
                    make(Opcode::Not, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_integer_arithmentic() {
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2 - 3",
                expected_constants: vec![1.into(), 2.into(), 3.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::Constant, &[2]),
                    make(Opcode::Subtract, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "2 * 3",
                expected_constants: vec![2.into(), 3.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Multiply, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "4 / 2",
                expected_constants: vec![4.into(), 2.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Divide, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_float_arithmentic() {
        let tests = vec![
            CompilerTestCase {
                input: "1.2 + .8 - 1.5",
                expected_constants: vec![1.2.into(), 0.8.into(), 1.5.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::Constant, &[2]),
                    make(Opcode::Subtract, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "3.2 * 0.5",
                expected_constants: vec![3.2.into(), 0.5.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Multiply, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1.6 / .4",
                expected_constants: vec![1.6.into(), 0.4.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Divide, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_string_operations() {
        let tests = vec![
            CompilerTestCase {
                input: r#""a" + "b""#,
                expected_constants: vec!["a".into(), "b".into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#""abc" sw "a""#,
                expected_constants: vec!["abc".into(), "a".into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::StartsWith, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#""abc" ew "c""#,
                expected_constants: vec!["abc".into(), "c".into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::EndsWith, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#""b" in "abc""#,
                expected_constants: vec!["b".into(), "abc".into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::In, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_array_operations() {
        let tests = vec![
            CompilerTestCase {
                input: r#"[1, .5, true, "abc"]"#,
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Float(0.5),
                    Object::String("abc".into()),
                ],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::True, &[]),
                    make(Opcode::Constant, &[2]),
                    make(Opcode::Array, &[4]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#"[x, x]"#,
                expected_constants: Vec::new(),
                expected_identifiers: vec!["x"],
                expected_instructions: vec![
                    make(Opcode::Global, &[0]),
                    make(Opcode::Global, &[0]),
                    make(Opcode::Array, &[2]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#"[[1]]"#,
                expected_constants: vec![Object::Integer(1)],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Array, &[1]),
                    make(Opcode::Array, &[1]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#"2 in [1, 2, 3]"#,
                expected_constants: vec![2.into(), 1.into(), 2.into(), 3.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Constant, &[2]),
                    make(Opcode::Constant, &[3]),
                    make(Opcode::Array, &[3]),
                    make(Opcode::In, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_cast_operations() {
        let tests = vec![CompilerTestCase {
            input: "x as float",
            expected_constants: Vec::new(),
            expected_identifiers: vec!["x".into()],
            expected_instructions: vec![
                make(Opcode::Global, &[0]),
                make(Opcode::Cast, &[code::CAST_FLOAT as usize]),
                make(Opcode::Pop, &[]),
            ],
        }];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_call_operations() {
        let tests = vec![CompilerTestCase {
            input: "len([1, 2])",
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_identifiers: vec!["len".into()],
            expected_instructions: vec![
                make(Opcode::Global, &[0]),
                make(Opcode::Constant, &[0]),
                make(Opcode::Constant, &[1]),
                make(Opcode::Array, &[2]),
                make(Opcode::Call, &[1]),
                make(Opcode::Pop, &[]),
            ],
        }];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_boolean_literals() {
        let tests = vec![
            CompilerTestCase {
                input: "true",
                expected_constants: Vec::new(),
                expected_identifiers: Vec::new(),
                expected_instructions: vec![make(Opcode::True, &[]), make(Opcode::Pop, &[])],
            },
            CompilerTestCase {
                input: "false",
                expected_constants: Vec::new(),
                expected_identifiers: Vec::new(),
                expected_instructions: vec![make(Opcode::False, &[]), make(Opcode::Pop, &[])],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_null_literal() {
        let test = CompilerTestCase {
            input: "null",
            expected_constants: Vec::new(),
            expected_identifiers: Vec::new(),
            expected_instructions: vec![make(Opcode::Null, &[]), make(Opcode::Pop, &[])],
        };
        let bytecode = compile(test.input);
        test.assert(&bytecode);
    }

    #[test]
    fn test_comparison_operators() {
        let tests = vec![
            CompilerTestCase {
                input: "1 == 2",
                expected_constants: vec![1.into(), 2.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Equal, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 != 2",
                expected_constants: vec![1.into(), 2.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::NotEqual, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 < 2",
                expected_constants: vec![1.into(), 2.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Less, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 <= 2",
                expected_constants: vec![1.into(), 2.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::LessOrEqual, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 > 2",
                expected_constants: vec![1.into(), 2.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Greater, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1 >= 2",
                expected_constants: vec![1.into(), 2.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::GreaterOrEqual, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_logical_operators() {
        let tests = vec![
            CompilerTestCase {
                input: "true and false",
                expected_constants: Vec::new(),
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::True, &[]),
                    make(Opcode::JumpNotTruthy, &[6]),
                    make(Opcode::Pop, &[]),
                    make(Opcode::False, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "true or false",
                expected_constants: Vec::new(),
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::True, &[]),
                    make(Opcode::JumpTruthy, &[6]),
                    make(Opcode::Pop, &[]),
                    make(Opcode::False, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_index_expressions() {
        let test = CompilerTestCase {
            input: "arr[0]",
            expected_constants: vec![0.into()],
            expected_identifiers: vec!["arr"],
            expected_instructions: vec![
                make(Opcode::Global, &[0]),
                make(Opcode::Constant, &[0]),
                make(Opcode::Index, &[]),
                make(Opcode::Pop, &[]),
            ],
        };
        let bytecode = compile(test.input);
        test.assert(&bytecode);
    }

    #[test]
    fn test_member_expressions() {
        let test = CompilerTestCase {
            input: "obj.field",
            expected_constants: Vec::new(),
            expected_identifiers: vec!["obj", "field"],
            expected_instructions: vec![
                make(Opcode::Global, &[0]),
                make(Opcode::Member, &[1]),
                make(Opcode::Pop, &[]),
            ],
        };
        let bytecode = compile(test.input);
        test.assert(&bytecode);
    }

    #[test]
    fn test_cast_types() {
        let tests = vec![
            CompilerTestCase {
                input: "x as int",
                expected_constants: Vec::new(),
                expected_identifiers: vec!["x"],
                expected_instructions: vec![
                    make(Opcode::Global, &[0]),
                    make(Opcode::Cast, &[code::CAST_INT as usize]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "x as string",
                expected_constants: Vec::new(),
                expected_identifiers: vec!["x"],
                expected_instructions: vec![
                    make(Opcode::Global, &[0]),
                    make(Opcode::Cast, &[code::CAST_STRING as usize]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "x as bool",
                expected_constants: Vec::new(),
                expected_identifiers: vec!["x"],
                expected_instructions: vec![
                    make(Opcode::Global, &[0]),
                    make(Opcode::Cast, &[code::CAST_BOOL as usize]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    fn compile(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expr = parser.parse().unwrap();

        let mut compiler = Compiler::new();
        compiler.compile(&expr).unwrap();
        compiler.program()
    }
}
