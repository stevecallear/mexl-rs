use std::{fmt::{self}, ops::{Deref, DerefMut}};

#[derive(Debug, Clone)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new() -> Self { Self(Vec::new()) }
}

impl Deref for Instructions {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        let mut i: usize = 0;
        while i < self.len() {
            let op: Opcode = self.0[i].into();
            let def = op.lookup();

            let (operands, offset) = read_operands(&def, &self.0[i + 1..]);
            out.push_str(format!("{:04} {}\n", i, format_instruction(&def, &operands)).as_str());

            i += 1 + offset;
        }
        write!(f, "{}", out)
    }
}

pub struct Definition {
    pub name: &'static str,
    pub operand_widths: &'static [usize],   
}

macro_rules! define_opcodes {
    ($($name:ident),*) => {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Opcode {
            $($name),* }        

        impl From<u8> for Opcode {
            fn from(v: u8) -> Self {
                match v {
                    $(x if x == Opcode::$name as u8 => Opcode::$name),*,
                    _ => panic!("unknown opcode: {}", v),
                }
            }
        }
    }
}

define_opcodes!(
    OpGlobal,
    OpConstant,
    OpArray,
    OpTrue,
    OpFalse,
    OpNull,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,	
	OpEqual,
	OpNotEqual,
	OpLess,
	OpLessOrEqual,
	OpGreater,
	OpGreaterOrEqual,
	OpStartsWith,
	OpEndsWith,
    OpIn,
    OpMinus,
	OpNot,
    OpMember,
    OpCall,
    OpCast,
    OpJumpTruthy,
    OpJumpNotTruthy,
    OpPop
);

pub const CAST_INT: u8 = 0;
pub const CAST_FLOAT: u8 = 1;
pub const CAST_STRING: u8 = 2;
pub const CAST_BOOL: u8 = 3;

impl Opcode {
    fn lookup(&self) -> Definition {
        match self {        
            Opcode::OpGlobal => Definition {
                name: "OpGlobal",
                operand_widths: &[2],
            },
            Opcode::OpConstant => Definition { 
                name: "OpConstant", 
                operand_widths: &[2],
            },
            Opcode::OpArray => Definition {
                name: "OpArray",
                operand_widths: &[2],
            },
            Opcode::OpTrue => Definition {
                name: "OpTrue", 
                operand_widths: &[],
            },
            Opcode::OpFalse => Definition {
                name: "OpFalse", 
                operand_widths: &[],
            },
            Opcode::OpNull => Definition {
                name: "OpNull", 
                operand_widths: &[],
            },
            Opcode::OpAdd => Definition {
                name: "OpAdd", 
                operand_widths: &[],
            },
            Opcode::OpSubtract => Definition {
                name: "OpSubtract", 
                operand_widths: &[],
            },
            Opcode::OpMultiply => Definition {
                name: "OpMultiply", 
                operand_widths: &[],
            },
            Opcode::OpDivide => Definition {
                name: "OpDivide", 
                operand_widths: &[],
            },          
            Opcode::OpEqual => Definition {
                name: "OpEqual",
                operand_widths: &[],
            },
            Opcode::OpNotEqual => Definition {
                name: "OpNotEqual",
                operand_widths: &[],
            },
            Opcode::OpLess => Definition {
                name: "OpLess",
                operand_widths: &[],
            },
            Opcode::OpLessOrEqual => Definition {
                name: "OpLessOrEqual",
                operand_widths: &[],
            },
            Opcode::OpGreater => Definition {
                name: "OpGreater",
                operand_widths: &[],
            },
            Opcode::OpGreaterOrEqual => Definition {
                name: "OpGreaterOrEqual",
                operand_widths: &[],
            },
            Opcode::OpStartsWith => Definition {
                name: "OpStartsWith",
                operand_widths: &[],
            },
            Opcode::OpEndsWith => Definition {
                name: "OpEndsWith",
                operand_widths: &[],
            },
            Opcode::OpIn => Definition {
                name: "OpIn",
                operand_widths: &[],
            },
            Opcode::OpNot => Definition {
                name: "OpNot", 
                operand_widths: &[],
            },
            Opcode::OpMinus => Definition {
                name: "OpMinus", 
                operand_widths: &[],
            },
            Opcode::OpMember => Definition {
                name: "OpMember",
                operand_widths: &[2],
            },
            Opcode::OpCall => Definition {
                name: "OpCall",
                operand_widths: &[2],
            },
            Opcode::OpCast => Definition {
                name: "OpCast",
                operand_widths: &[1],
            },
            Opcode::OpJumpTruthy => Definition {
                name: "OpJumpTruthy",
                operand_widths: &[2],
            },
            Opcode::OpJumpNotTruthy => Definition {
                name: "OpJumpNotTruthy",
                operand_widths: &[2],
            },
            Opcode::OpPop => Definition {
                name: "OpPop", 
                operand_widths: &[],
            },
        }
    }
}

pub fn make(op: Opcode, operands: &[usize]) -> Instructions {
    let def = op.lookup();
    
    let instruction_len = 1 + def.operand_widths.iter().sum::<usize>();
    let mut instruction = Vec::with_capacity(instruction_len);
    instruction.push(op as u8);

    for (i, &o) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                let val = o as u16;
                instruction.extend_from_slice(&val.to_be_bytes());
            },
            1 => {
                instruction.push(o as u8)
            },
            _ => panic!("operand width {} not supported", width)
        }
    }

    Instructions(instruction)
}

pub fn read_operands(def: &Definition, instructions: &[u8]) -> (Vec<usize>, usize) {
    let mut operands = Vec::with_capacity(def.operand_widths.len());
    let mut offset = 0;

    for &width in def.operand_widths {
        match width {
            2 => {
                let slice = &instructions[offset..offset+2];
                let arr: [u8; 2] = slice.try_into().unwrap();
                let val = u16::from_be_bytes(arr);
                
                operands.push(val as usize);
                offset += 2;
            },
            1 => {
                let val = instructions[offset];
                operands.push(val as usize);
                offset += 1;
            },
            _ => panic!("operand width {} not supported", width)
        }
    }

    (operands, offset)
}

fn format_instruction(def: &Definition, operands: &[usize]) -> String {
    let count = def.operand_widths.len();
    if operands.len() != count {
        return format!("ERROR: operand count mismatch: {}, {}", operands.len(), count);
    }
    match count {
        0 => def.name.to_string(),
        1 => format!("{} {}", def.name.to_string(), operands[0]),
        _ => format!("ERROR: unhandled operand count: {}", count),
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn test_instructions_fmt() {
        let tests: Vec<(Instructions, &str)> = vec!{
            (make(Opcode::OpGlobal, &[1]), "0000 OpGlobal 1"),
            (make(Opcode::OpConstant, &[1]), "0000 OpConstant 1"),
            (make(Opcode::OpArray, &[1]), "0000 OpArray 1"),
            (make(Opcode::OpTrue, &[]), "0000 OpTrue"),
            (make(Opcode::OpFalse, &[]), "0000 OpFalse"),
            (make(Opcode::OpNull, &[]), "0000 OpNull"),
            (make(Opcode::OpAdd, &[]), "0000 OpAdd"),
            (make(Opcode::OpSubtract, &[]), "0000 OpSubtract"),
            (make(Opcode::OpMultiply, &[]), "0000 OpMultiply"),
            (make(Opcode::OpDivide, &[]), "0000 OpDivide"),
            (make(Opcode::OpEqual, &[]), "0000 OpEqual"),
            (make(Opcode::OpNotEqual, &[]), "0000 OpNotEqual"),
            (make(Opcode::OpLess, &[]), "0000 OpLess"),
            (make(Opcode::OpLessOrEqual, &[]), "0000 OpLessOrEqual"),
            (make(Opcode::OpGreater, &[]), "0000 OpGreater"),
            (make(Opcode::OpGreaterOrEqual, &[]), "0000 OpGreaterOrEqual"),
            (make(Opcode::OpStartsWith, &[]), "0000 OpStartsWith"),
            (make(Opcode::OpEndsWith, &[]), "0000 OpEndsWith"),
            (make(Opcode::OpIn, &[]), "0000 OpIn"),
            (make(Opcode::OpMinus, &[]), "0000 OpMinus"),
            (make(Opcode::OpNot, &[]), "0000 OpNot"),
            (make(Opcode::OpMember, &[1]), "0000 OpMember 1"),
            (make(Opcode::OpCall, &[1]), "0000 OpCall 1"),
            (make(Opcode::OpCast, &[CAST_INT as usize]), "0000 OpCast 0"),
            (make(Opcode::OpJumpTruthy, &[1]), "0000 OpJumpTruthy 1"),
            (make(Opcode::OpJumpNotTruthy, &[1]), "0000 OpJumpNotTruthy 1"),
            (make(Opcode::OpPop, &[]), "0000 OpPop"),
        };

        for (instructions, expected) in tests {
            let actual = format!("{}", instructions);
            assert_eq!(actual.trim_end(), expected);
        }
    }

    #[test]
    fn test_instructions_fmt_complex() {
        let input = "1.0 - x as float gt 0 and y in [\"a\", \"b\"]";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expr = parser.parse().unwrap();
        
        let mut compiler = Compiler::new();
        compiler.compile(&expr).unwrap();
        
        let instructions = compiler.program().instructions;
        let actual = format!("{}", instructions);

        let expected = r#"0000 OpConstant 0
0003 OpGlobal 0
0006 OpCast 1
0008 OpSubtract
0009 OpConstant 1
0012 OpGreater
0013 OpJumpNotTruthy 30
0016 OpPop
0017 OpGlobal 1
0020 OpConstant 2
0023 OpConstant 3
0026 OpArray 2
0029 OpIn
0030 OpPop
"#;
        assert_eq!(actual, expected);
    }
    
}