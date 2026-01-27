use crate::{Environment, builtin, code::{self, Instructions, Opcode}, compiler::Program, object::{self, Function, Object}};

const STACK_SIZE: usize = 2048;

pub struct VM {
    instructions: Instructions,
    constants: Vec<Object>,
    identifiers: Vec<String>,
    stack: Vec<Object>,
    sp: usize,
    last_popped_elem: Option<Object>,
}

impl VM {
    pub fn new(program: Program) -> Self {
        Self {
            instructions: program.instructions,
            constants: program.constants,
            identifiers: program.identifiers,
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
            last_popped_elem: None,
        }
    }

    pub fn run(&mut self, env: &Environment) -> Result<Object, String> {
        let mut ip = 0;

        while ip < self.instructions.len() {
            let op = Opcode::from(self.instructions[ip]);
            ip += 1;

            match op {
                Opcode::OpGlobal => {
                    let index = self.read_usize(&mut ip);
                    let ident = self.identifiers[index].clone();
                    self.execute_identifier(&ident, env)?;
                },
                Opcode::OpConstant => {
                    let index = self.read_usize(&mut ip);
                    let obj = self.constants[index].clone();
                    self.push(obj)?;
                },
                Opcode::OpArray => {
                    let array_len = self.read_usize(&mut ip);
                    self.execute_array(array_len)?;
                },
                Opcode::OpAdd | Opcode::OpSubtract | Opcode::OpMultiply | Opcode::OpDivide |
                Opcode::OpEqual | Opcode::OpNotEqual |
                Opcode::OpLess | Opcode::OpLessOrEqual | Opcode::OpGreater | Opcode::OpGreaterOrEqual |
                Opcode::OpStartsWith | Opcode::OpEndsWith | Opcode::OpIn => {
                    let right = self.pop()?;
                    let left = self.pop()?;

                    let result = self.execute_binary_operation(op, left, right)?;
                    self.push(result)?;
                }
                Opcode::OpNot => self.execute_not_operation()?,
                Opcode::OpMinus => self.execute_minus_operation()?,
                Opcode::OpTrue => self.push(Object::Boolean(true))?,
                Opcode::OpFalse => self.push(Object::Boolean(false))?,
                Opcode::OpNull => self.push(Object::Null)?,
                Opcode::OpMember => {
                    let index = self.read_usize(&mut ip);
                    self.execute_member_operation(index)?;
                },
                Opcode::OpCast => {
                    let type_code = self.read_u8(&mut ip);
                    self.execute_cast_operation(type_code)?;
                },
                Opcode::OpCall => {
                    let num_args = self.read_usize(&mut ip);
                    self.execute_call_operation(num_args)?;
                },
                Opcode::OpJumpTruthy | Opcode::OpJumpNotTruthy => {
                    self.execute_jump_operation(op, &mut ip)?;
                },
                Opcode::OpPop => { 
                    self.pop()?; 
                },                
            }
        }

        match &self.last_popped_elem {
            Some(obj) => Ok(obj.clone()),
            None => Ok(Object::Null),
        }
    }

    fn read_u8(&self, ip: &mut usize) -> u8 {
        let n = u8::from_be_bytes([self.instructions[*ip]]);
        *ip += 1;
        n
    }

    fn read_usize(&self, ip: &mut usize) -> usize {
        let n = u16::from_be_bytes([
            self.instructions[*ip],
            self.instructions[*ip + 1],
        ]) as usize;
        *ip += 2;
        n
    }

    fn execute_identifier(&mut self, ident: &str, env: &Environment) -> Result<(), String> {
        let obj = match env.get(ident) {
            Some(obj) => obj,
            None => {
                match get_builtin(ident) {
                    Some(obj) => obj,
                    None => Object::Null,
                }
            }
        };
        self.push(obj)?;
        Ok(())
    }

    fn execute_array(&mut self, array_len: usize) -> Result<(), String> {
        let mut objs = Vec::with_capacity(array_len);
        for _ in 0..array_len {
            let obj = self.pop()?;
            objs.push(obj);
        }
        let array = Object::Array(objs);
        self.push(array)?;
        Ok(())
    }

    fn execute_member_operation(&mut self, ident_index: usize) -> Result<(), String> {
        let left = self.pop()?;
        
        let ident = &self.identifiers[ident_index].clone();
        let obj = match left {
            Object::Map(m) => {
                match m.get(ident) {
                    Some(o) => o.clone(),
                    None => Object::Null,
                }
            },
            Object::Null => Object::Null,
            _ => return Err(format!("invalid container type: {:?}", left))
        };

        self.push(obj)
    }

    fn execute_cast_operation(&mut self, type_code: u8) -> Result<(), String> {
        let left = self.pop()?;
        let obj = match type_code {
            code::CAST_INT => object::cast_to_integer(left)?,
            code::CAST_FLOAT => object::cast_to_float(left)?,
            code::CAST_STRING => object::cast_to_string(left)?,
            code::CAST_BOOL => object::cast_to_boolean(left)?,
            _ => return Err(format!("invalid cast type: {}", type_code))
        };
        self.push(obj)
    }

    fn execute_call_operation(&mut self, num_args: usize) -> Result<(), String> {
        let mut args = Vec::with_capacity(num_args);
        for _ in 0..num_args {
            let obj = self.pop()?;
            args.push(obj);
        }

        let obj = self.pop()?;
        match obj {
            Object::Function(f) => {
                let result = (f.handler)(args)?;
                self.push(result)?;
            },
            _ => return Err(format!("invalid function type: {:?}", obj)),
        }

        Ok(())
    }

    fn execute_binary_operation(&self, op: Opcode, left: Object, right: Object) -> Result<Object, String> {
        let (unified_left, unified_right) = object::unify_operands(left, right);

        match (op, unified_left, unified_right) {
            (_, Object::Integer(l), Object::Integer(r)) => {
                self.execute_integer_binary_operation(op, l, r)
            },
            (_, Object::Float(l), Object::Float(r)) => {
                self.execute_float_binary_operation(op, l, r)
            },
            (_, Object::String(l), Object::String(r)) => {
                self.execute_string_binary_operation(op, l.as_str(), r.as_str())
            },
            (_, Object::Boolean(l), Object::Boolean(r)) => {
                self.execute_boolean_binary_operation(op, l, r)
            },
            (Opcode::OpIn, left, Object::Array(r)) => {
                let found = r.iter().any(|e| Self::apply_loose_equality(&left, e));
                Ok(Object::Boolean(found))
            },
            _ => Err("type mismatch".into()),
        }
    }

    fn execute_integer_binary_operation(&self, op: Opcode, left: i64, right: i64) -> Result<Object, String> {
        let result = match op {
            Opcode::OpAdd => Object::Integer(left + right),
            Opcode::OpSubtract => Object::Integer(left - right),
            Opcode::OpMultiply => Object::Integer(left * right),
            Opcode::OpDivide => Object::Integer(left / right),
            Opcode::OpEqual => Object::Boolean(left == right),
            Opcode::OpNotEqual => Object::Boolean(left != right),
            Opcode::OpLess => Object::Boolean(left < right),
            Opcode::OpLessOrEqual => Object::Boolean(left <= right),
            Opcode::OpGreater => Object::Boolean(left > right),
            Opcode::OpGreaterOrEqual => Object::Boolean(left >= right),
            _ => return Err(format!("unknown integer operation: {:?}", op)),
        };
        Ok(result)
    }

    fn execute_float_binary_operation(&self, op: Opcode, left: f64, right: f64) -> Result<Object, String> {
        let result = match op {
            Opcode::OpAdd => Object::Float(left + right),
            Opcode::OpSubtract => Object::Float(left - right),
            Opcode::OpMultiply => Object::Float(left * right),
            Opcode::OpDivide => Object::Float(left / right),
            Opcode::OpEqual => Object::Boolean(left == right),
            Opcode::OpNotEqual => Object::Boolean(left != right),
            Opcode::OpLess => Object::Boolean(left < right),
            Opcode::OpLessOrEqual => Object::Boolean(left <= right),
            Opcode::OpGreater => Object::Boolean(left > right),
            Opcode::OpGreaterOrEqual => Object::Boolean(left >= right),
            _ => return Err(format!("unknown float operation: {:?}", op)),
        };
        Ok(result)
    }

    fn execute_string_binary_operation(&self, op: Opcode, left: &str, right: &str) -> Result<Object, String> {
        let result = match op {
            Opcode::OpAdd => {
                  let mut str = left.to_owned();
                  str.push_str(right);
                  Object::String(str)
            },
            Opcode::OpEqual => Object::Boolean(left == right),
            Opcode::OpNotEqual => Object::Boolean(left != right),
            Opcode::OpStartsWith => Object::Boolean(left.starts_with(right)),        
            Opcode::OpEndsWith => Object::Boolean(left.ends_with(right)),
            Opcode::OpIn => Object::Boolean(right.contains(left)),
            _ => return Err(format!("unknown string operation: {:?}", op)),
        };
        Ok(result)
    }

    fn execute_boolean_binary_operation(&self, op: Opcode, left: bool, right: bool) -> Result<Object, String> {
        let result = match op {
            Opcode::OpEqual => Object::Boolean(left == right),
            Opcode::OpNotEqual => Object::Boolean(left != right),
            // Opcode::OpAnd => Object::Boolean(left && right),
            // Opcode::OpOr => Object::Boolean(left || right),
            _ => return Err(format!("unknown boolean operation: {:?}", op)),
        };
        Ok(result)
    }

     fn execute_not_operation(&mut self) -> Result<(), String> {
        match self.pop()? {
            Object::Boolean(b) => self.push(Object::Boolean(!b))?,
            Object::Null => self.push(Object::Boolean(true))?,
            _ => self.push(Object::Boolean(false))?, // default to false for non-boolean
        };
        Ok(())
    }

    fn execute_minus_operation(&mut self) -> Result<(), String> {
        let obj = self.pop()?;
        match obj {
            Object::Integer(i) => self.push(Object::Integer(-i))?,
            Object::Float(f) => self.push(Object::Float(-f))?,
            Object::Null => self.push(Object::Null)?,
            _ => return Err("unsupported type for negation".into()),
        }
        Ok(())
    }

    fn execute_jump_operation(&mut self, op: Opcode, ip: &mut usize) -> Result<(), String> {
        let pos = self.read_usize(ip);
        let condition = self.stack.last().ok_or("stack underflow")?;

        let jump = match op {
            Opcode::OpJumpTruthy => object::is_truthy(condition),
            Opcode::OpJumpNotTruthy => !object::is_truthy(condition),
            _ => return Err(format!("invalid jump opcode: {:?}", op)),
        };
        
        if jump {
            println!("jump to {}", pos);
            *ip = pos
        }

        Ok(())
    }
    
    fn apply_loose_equality(left: &Object, right: &Object) -> bool {
        match (left, right) {
            (Object::Integer(l), Object::Float(r)) => *l as f64 == *r,
            (Object::Float(l), Object::Integer(r)) => *l == *r as f64,
            (l, r) => l == r
        }
    }

    fn push(&mut self, obj: Object) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            return Err("stack overflow".into());
        }

        self.stack.push(obj);
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Result<Object, String> {
        if self.sp == 0 {
            return Err("stack underflow".into());
        }

        let obj = self.stack.pop().ok_or("stack underflow")?;
        self.last_popped_elem = Some(obj.clone());
        self.sp -= 1;

        Ok(obj)
    }
}

fn get_builtin(ident: &str) -> Option<Object> {    
    match ident {
        "len" => Some(Object::Function(Function {
                name: "len".to_owned(),
                handler: builtin::len,
        })),
        "lower" => Some(Object::Function(Function {
                name: "lower".to_owned(),
                handler: builtin::lower,
        })),
        "upper" => Some(Object::Function(Function {
                name: "upper".to_owned(),
                handler: builtin::upper,
        })),
        _ => None
    }
}

#[cfg(test)] 
mod test {
    use crate::{Environment, compiler::Compiler, lexer::Lexer, parser::Parser, tests::fixtures};
    use super::*;

    #[test]
    fn test_run_integer_expressions() {
        for test in fixtures::integer_tests() {
            let env = Environment::default();
            let actual = run(test.input, &env);
            assert_eq!(actual, test.expected, "({}): got {}, expected {}", test.input, actual, test.expected);
        }
    }

    #[test]
    fn test_run_float_expressions() {
        for test in fixtures::float_tests() {
            let env = Environment::default();
            let actual = run(test.input, &env);
            assert_eq!(actual, test.expected, "({}): got {}, expected {}", test.input, actual, test.expected);
        }
    }

    #[test]
    fn test_run_string_expressions() {
         for test in fixtures::string_tests() {
            let env = Environment::default();
            let actual = run(test.input, &env);
            assert_eq!(actual, test.expected, "({}): got {}, expected {}", test.input, actual, test.expected);
        }
    }

    #[test]
    fn test_run_boolean_expressions() {
        for test in fixtures::boolean_tests() {
            let env = Environment::default();
            let actual = run(test.input, &env);
            assert_eq!(actual, test.expected, "({}): got {}, expected {}", test.input, actual, test.expected);
        }
    }

    #[test]
    fn test_run_ident_expressions() {
        let mut env = Environment::default();
        for test in fixtures::ident_tests(&mut env) {
            let actual = run(test.input, &env);
            assert_eq!(actual, test.expected, "({}): got {}, expected {}", test.input, actual, test.expected);
        }
    }

     #[test]
    fn test_run_builtin_expressions() {
        let env = Environment::default();
        for test in fixtures::builtin_tests() {
            let actual = run(test.input, &env);
            assert_eq!(actual, test.expected, "({}): got {}, expected {}", test.input, actual, test.expected);
        }
    }

     #[test]
    fn test_run_cast_expressions() {
        let env = Environment::default();
        for test in fixtures::cast_tests() {
            let actual = run(test.input, &env);
            assert_eq!(actual, test.expected, "({}): got {}, expected {}", test.input, actual, test.expected);
        }
    }

     #[test]
    fn test_run_member_expressions() {
        let mut env = Environment::default();
        for test in fixtures::member_tests(&mut env) {
            let actual = run(test.input, &env);
            assert_eq!(actual, test.expected, "({}): got {}, expected {} ({:?})", test.input, actual, test.expected, env);
        }
    }

    fn run(input: &str, env: &Environment) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expr = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        compiler.compile(&expr).unwrap();
        
        let mut vm = VM::new(compiler.program());
        vm.run(env).unwrap()
    }
}