use crate::{Object, ast::{CastExpression, Expression, InfixExpression, PrefixExpression}, code::{self, Instructions, Opcode}, token::TokenType};

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    identifiers: Vec<String>,
}

#[derive(Clone)]
pub struct Program {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
    pub identifiers: Vec<String>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: Vec::new(),
            identifiers: Vec::new(),
        }
    }

    pub fn program(self) -> Program {
        Program { 
            instructions: self.instructions,
            constants: self.constants,
            identifiers: self.identifiers,
        }
    }

    pub fn compile(&mut self, node: &Expression) -> Result<(), String> {
        self.compile_expression(node)?;
        self.emit(Opcode::OpPop, &[]);
        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<(), String> {
        match expr {
            Expression::Infix(e) => {
                self.compile_infix_expression(&e)?;
            },
            Expression::Prefix(e) => {
                self.compile_prefix_expression(&e)?;
            },
            Expression::IntegerLiteral(v) => {
                let obj = Object::Integer(*v);
                let idx = self.add_constant(obj);
                self.emit(Opcode::OpConstant, &[idx]);
            },
            Expression::FloatLiteral(v) => {
                let obj = Object::Float(*v);
                let idx = self.add_constant(obj);
                self.emit(Opcode::OpConstant, &[idx]);
            },
            Expression::StringLiteral(v) => {
                let obj = Object::String(v.clone());
                let idx = self.add_constant(obj);
                self.emit(Opcode::OpConstant, &[idx]);
            },           
            Expression::ArrayLiteral(v) => {
                self.compile_expressions(v)?;
                self.emit(Opcode::OpArray, &[v.len()]);
            },
            Expression::Boolean(v) => {
                let op = match v {
                    true => Opcode::OpTrue,
                    false => Opcode::OpFalse,
                };
                self.emit(op, &[]);
            },
            Expression::Null => {
                self.emit(Opcode::OpNull, &[]);
            },
            Expression::Identifier(v) => {
                let idx = self.add_identifier(v.value.to_owned());
                self.emit(Opcode::OpGlobal, &[idx]);
            },
            Expression::Call(e) => {
                self.compile_expression(&e.function)?;
                self.compile_expressions(&e.arguments)?;
                self.emit(Opcode::OpCall, &[e.arguments.len()]);
            },            
            Expression::Member(e) => {
                self.compile_expression(&e.left)?;
                let idx = self.add_identifier(e.member.value.clone());
                self.emit(Opcode::OpMember, &[idx]);
            },
            Expression::Cast(e) => {
                self.compile_cast_expression(&e)?;
            }
        };
        Ok(())
    }

    fn compile_expressions(&mut self, exprs: &[Expression]) -> Result<(), String> {
        for expr in exprs {
            self.compile_expression(expr)?;
        }
        Ok(())
    }

    fn emit_jump_placeholder(&mut self, op: Opcode) -> usize {
        let pos = self.instructions.len();
        self.emit(op, &[9999]);
        pos
    }

    fn patch(&mut self, offset: usize, target: usize) -> Result<(), String> {
        let operand_pos = offset + 1;
        if operand_pos + 2 > self.instructions.len() {
            return Err("undefined jump target".into());
        }

        let bytes = (target as u16).to_be_bytes();
        self.instructions[operand_pos] = bytes[0];
        self.instructions[operand_pos + 1] = bytes[1];

        Ok(())
    }

    fn compile_logical_op(&mut self, expr: &InfixExpression, jump_op: Opcode) -> Result<(), String> {
        self.compile_expression(&expr.left)?;
            
        let jump_pos = self.emit_jump_placeholder(jump_op);

        self.emit(Opcode::OpPop, &[]);
        
        self.compile_expression(&expr.right)?;
        
        let after_right_pos = self.instructions.len();
        self.patch(jump_pos, after_right_pos)
    }

    fn compile_infix_expression(&mut self, expr: &InfixExpression) -> Result<(), String> {
        match expr.token.token_type {
            TokenType::And => {
                return self.compile_logical_op(expr, Opcode::OpJumpNotTruthy);
            },
            TokenType::Or => {
                return self.compile_logical_op(expr, Opcode::OpJumpTruthy);
            },
            _ => {} // continue
        }

        self.compile_expression(&expr.left)?;
        self.compile_expression(&expr.right)?;
        let op = match expr.token.token_type {
            TokenType::Plus => Opcode::OpAdd,
            TokenType::Minus => Opcode::OpSubtract,
            TokenType::Asterisk => Opcode::OpMultiply,
            TokenType::Slash => Opcode::OpDivide,
            TokenType::Equal => Opcode::OpEqual,
            TokenType::NotEqual => Opcode::OpNotEqual,
            TokenType::LessThan => Opcode::OpLess,
            TokenType::LessThanEqual => Opcode::OpLessOrEqual,
            TokenType::GreaterThan => Opcode::OpGreater,
            TokenType::GreaterThanEqual => Opcode::OpGreaterOrEqual,
            TokenType::StartsWith => Opcode::OpStartsWith,
            TokenType::EndsWith => Opcode::OpEndsWith,
            TokenType::In => Opcode::OpIn,
            _ => return Err(format!("unknown infix operator: {}", expr.operator)),
        };
        self.emit(op, &[]);
        Ok(())
    }

    fn compile_prefix_expression(&mut self, expr: &PrefixExpression) -> Result<(), String> {
        self.compile_expression(&expr.right)?;
        match expr.token.token_type {
            TokenType::Bang => self.emit(Opcode::OpNot, &[]),
            TokenType::Minus => self.emit(Opcode::OpMinus, &[]),
            _ => return Err(format!("unknown prefix operator: {}", expr.operator)),
        };
        Ok(())
    }

    fn compile_cast_expression(&mut self, expr: &CastExpression) -> Result<(), String> {
        self.compile_expression(&expr.left)?;
        let type_code = match expr.target_type.value.as_str() {
            "int" => code::CAST_INT,
            "float" => code::CAST_FLOAT,
            "string" => code::CAST_STRING,
            "bool" => code::CAST_BOOL,
            other => return Err(format!("unknown cast target: {}", other))
        };
        self.emit(Opcode::OpCast, &[type_code as usize]);
        Ok(())
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn add_identifier(&mut self, ident: String) -> usize {
        match self.identifiers.iter().position(|i| *i == ident) {
            Some(i) => i,
            None => {
                self.identifiers.push(ident);
                self.identifiers.len() - 1
            },
        }
    }

    fn emit(&mut self, op: Opcode, operands: &[usize]) -> usize {
        let instruction = code::make(op, operands);
        let pos = self.instructions.len();
        self.instructions.extend(instruction.iter());
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
            let expected_instructions: Vec<u8> = self.expected_instructions.iter().flat_map(|x| x.deref().clone()).collect();
            assert_eq!(*actual.instructions, expected_instructions, "instructions do not match");
            assert_eq!(actual.constants, self.expected_constants, "constants do not match");
            assert_eq!(actual.identifiers, self.expected_identifiers, "identifiers do not match");
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
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpMinus, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "!true",
                expected_constants: Vec::new(),
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                        make(Opcode::OpTrue, &[]),
                    make(Opcode::OpNot, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "not true",
                expected_constants: Vec::new(),
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpTrue, &[]),
                    make(Opcode::OpNot, &[]),
                    make(Opcode::OpPop, &[]),
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
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpAdd, &[]),
                    make(Opcode::OpConstant, &[2]),
                    make(Opcode::OpSubtract, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },           
            CompilerTestCase {
                input: "2 * 3",
                expected_constants: vec![2.into(), 3.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpMultiply, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "4 / 2",
                expected_constants: vec![4.into(), 2.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpDivide, &[]),
                    make(Opcode::OpPop, &[]),
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
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpAdd, &[]),
                    make(Opcode::OpConstant, &[2]),
                    make(Opcode::OpSubtract, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },           
            CompilerTestCase {
                input: "3.2 * 0.5",
                expected_constants: vec![3.2.into(), 0.5.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpMultiply, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1.6 / .4",
                expected_constants: vec![1.6.into(), 0.4.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpDivide, &[]),
                    make(Opcode::OpPop, &[]),
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
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpAdd, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#""abc" sw "a""#,
                expected_constants: vec!["abc".into(), "a".into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpStartsWith, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#""abc" ew "c""#,
                expected_constants: vec!["abc".into(), "c".into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpEndsWith, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#""b" in "abc""#,
                expected_constants: vec!["b".into(), "abc".into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpIn, &[]),
                    make(Opcode::OpPop, &[]),
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
                expected_constants: vec![Object::Integer(1), Object::Float(0.5), Object::String("abc".into())],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpTrue, &[]),
                    make(Opcode::OpConstant, &[2]),
                    make(Opcode::OpArray, &[4]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#"[x, x]"#,
                expected_constants: Vec::new(),
                expected_identifiers: vec!["x"],
                expected_instructions: vec![
                    make(Opcode::OpGlobal, &[0]),
                    make(Opcode::OpGlobal, &[0]),
                    make(Opcode::OpArray, &[2]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#"[[1]]"#,
                expected_constants: vec![Object::Integer(1)],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpArray, &[1]),
                    make(Opcode::OpArray, &[1]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerTestCase {
                input: r#"2 in [1, 2, 3]"#,
                expected_constants: vec![2.into(), 1.into(), 2.into(), 3.into()],
                expected_identifiers: Vec::new(),
                expected_instructions: vec![
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpConstant, &[2]),
                    make(Opcode::OpConstant, &[3]),
                    make(Opcode::OpArray, &[3]),
                    make(Opcode::OpIn, &[]),
                    make(Opcode::OpPop, &[]),
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
        let tests = vec![
            CompilerTestCase {
                input: "x as float",
                expected_constants: Vec::new(),
                expected_identifiers: vec!["x".into()],
                expected_instructions: vec![
                    make(Opcode::OpGlobal, &[0]),
                    make(Opcode::OpCast, &[code::CAST_FLOAT as usize]),
                    make(Opcode::OpPop, &[]),
                ],
            },
        ];
        
        for test in tests {
            let bytecode = compile(test.input);
            test.assert(&bytecode);
        }
    }

    #[test]
    fn test_call_operations() {
        let tests = vec![
            CompilerTestCase {
                input: "len([1, 2])",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_identifiers: vec!["len".into()],
                expected_instructions: vec![
                    make(Opcode::OpGlobal, &[0]),
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpArray, &[2]),
                    make(Opcode::OpCall, &[1]),
                    make(Opcode::OpPop, &[]),
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

