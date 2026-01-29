use std::{
    fmt::{self},
    ops::{Deref, DerefMut},
};

/// Represents a sequence of bytecode instructions.
#[derive(Debug, Clone)]
pub struct Instructions(pub Vec<u8>);

impl Default for Instructions {
    /// Creates a default, empty Instructions instance.
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl Deref for Instructions {
    /// The target type when dereferencing Instructions.
    type Target = Vec<u8>;

    /// Dereferences the Instructions to access the underlying byte vector.
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Instructions {
    /// Dereferences the Instructions to access the underlying byte vector mutably.
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Display for Instructions {
    /// Formats the Instructions as a human-readable string.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut i = 0;
        while i < self.len() {
            let op: Opcode = self.0[i].into();
            let def = op.lookup();

            let (operands, offset) = read_operands(&def, &self.0[i + 1..]);
            writeln!(f, "{:04} {}", i, format_instruction(&def, &operands))?;

            i += 1 + offset;
        }
        Ok(())
    }
}

/// Represents the definition of an opcode, including its name and operand widths.
pub struct Definition {
    pub name: &'static str,
    pub operand_widths: &'static [usize],
}

// Shared constants for operand widths to avoid repeated allocations
const NO_OPERANDS: &[usize] = &[];
const ONE_OPERAND: &[usize] = &[2];
const ONE_BYTE_OPERAND: &[usize] = &[1];

/// Defines the opcodes used in the bytecode.
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
    Global,
    Constant,
    Array,
    True,
    False,
    Null,
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    StartsWith,
    EndsWith,
    In,
    Minus,
    Not,
    Member,
    Call,
    Cast,
    JumpTruthy,
    JumpNotTruthy,
    Pop
);

// Cast type codes
pub const CAST_INT: u8 = 0;
pub const CAST_FLOAT: u8 = 1;
pub const CAST_STRING: u8 = 2;
pub const CAST_BOOL: u8 = 3;

impl Opcode {
    /// Looks up the definition of the opcode.
    fn lookup(&self) -> Definition {
        match self {
            Opcode::Global => Definition {
                name: "Global",
                operand_widths: ONE_OPERAND,
            },
            Opcode::Constant => Definition {
                name: "Constant",
                operand_widths: ONE_OPERAND,
            },
            Opcode::Array => Definition {
                name: "Array",
                operand_widths: ONE_OPERAND,
            },
            Opcode::True => Definition {
                name: "True",
                operand_widths: NO_OPERANDS,
            },
            Opcode::False => Definition {
                name: "False",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Null => Definition {
                name: "Null",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Add => Definition {
                name: "Add",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Subtract => Definition {
                name: "Subtract",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Multiply => Definition {
                name: "Multiply",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Divide => Definition {
                name: "Divide",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Equal => Definition {
                name: "Equal",
                operand_widths: NO_OPERANDS,
            },
            Opcode::NotEqual => Definition {
                name: "NotEqual",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Less => Definition {
                name: "Less",
                operand_widths: NO_OPERANDS,
            },
            Opcode::LessOrEqual => Definition {
                name: "LessOrEqual",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Greater => Definition {
                name: "Greater",
                operand_widths: NO_OPERANDS,
            },
            Opcode::GreaterOrEqual => Definition {
                name: "GreaterOrEqual",
                operand_widths: NO_OPERANDS,
            },
            Opcode::StartsWith => Definition {
                name: "StartsWith",
                operand_widths: NO_OPERANDS,
            },
            Opcode::EndsWith => Definition {
                name: "EndsWith",
                operand_widths: NO_OPERANDS,
            },
            Opcode::In => Definition {
                name: "In",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Not => Definition {
                name: "Not",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Minus => Definition {
                name: "Minus",
                operand_widths: NO_OPERANDS,
            },
            Opcode::Member => Definition {
                name: "Member",
                operand_widths: ONE_OPERAND,
            },
            Opcode::Call => Definition {
                name: "Call",
                operand_widths: ONE_OPERAND,
            },
            Opcode::Cast => Definition {
                name: "Cast",
                operand_widths: ONE_BYTE_OPERAND,
            },
            Opcode::JumpTruthy => Definition {
                name: "JumpTruthy",
                operand_widths: ONE_OPERAND,
            },
            Opcode::JumpNotTruthy => Definition {
                name: "JumpNotTruthy",
                operand_widths: ONE_OPERAND,
            },
            Opcode::Pop => Definition {
                name: "Pop",
                operand_widths: NO_OPERANDS,
            },
        }
    }
}

/// Creates a bytecode instruction for the given opcode and operands.
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
            }
            1 => instruction.push(o as u8),
            _ => panic!("operand width {} not supported", width),
        }
    }

    Instructions(instruction)
}

/// Reads operands from the instruction byte slice based on the definition.
pub fn read_operands(def: &Definition, instructions: &[u8]) -> (Vec<usize>, usize) {
    let mut operands = Vec::with_capacity(def.operand_widths.len());
    let mut offset = 0;

    for &width in def.operand_widths {
        let value = match width {
            2 => {
                let arr: [u8; 2] = instructions[offset..offset + 2].try_into().unwrap();
                u16::from_be_bytes(arr) as usize
            }
            1 => instructions[offset] as usize,
            _ => panic!("operand width {} not supported", width),
        };
        operands.push(value);
        offset += width;
    }

    (operands, offset)
}

/// Formats an instruction into a human-readable string.
fn format_instruction(def: &Definition, operands: &[usize]) -> String {
    let count = def.operand_widths.len();
    if operands.len() != count {
        return format!(
            "ERROR: operand count mismatch: {}, {}",
            operands.len(),
            count
        );
    }
    match count {
        0 => def.name.to_string(),
        1 => format!("{} {}", def.name, operands[0]),
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
        let tests: Vec<(Instructions, &str)> = vec![
            (make(Opcode::Global, &[1]), "0000 Global 1"),
            (make(Opcode::Constant, &[1]), "0000 Constant 1"),
            (make(Opcode::Array, &[1]), "0000 Array 1"),
            (make(Opcode::True, &[]), "0000 True"),
            (make(Opcode::False, &[]), "0000 False"),
            (make(Opcode::Null, &[]), "0000 Null"),
            (make(Opcode::Add, &[]), "0000 Add"),
            (make(Opcode::Subtract, &[]), "0000 Subtract"),
            (make(Opcode::Multiply, &[]), "0000 Multiply"),
            (make(Opcode::Divide, &[]), "0000 Divide"),
            (make(Opcode::Equal, &[]), "0000 Equal"),
            (make(Opcode::NotEqual, &[]), "0000 NotEqual"),
            (make(Opcode::Less, &[]), "0000 Less"),
            (make(Opcode::LessOrEqual, &[]), "0000 LessOrEqual"),
            (make(Opcode::Greater, &[]), "0000 Greater"),
            (make(Opcode::GreaterOrEqual, &[]), "0000 GreaterOrEqual"),
            (make(Opcode::StartsWith, &[]), "0000 StartsWith"),
            (make(Opcode::EndsWith, &[]), "0000 EndsWith"),
            (make(Opcode::In, &[]), "0000 In"),
            (make(Opcode::Minus, &[]), "0000 Minus"),
            (make(Opcode::Not, &[]), "0000 Not"),
            (make(Opcode::Member, &[1]), "0000 Member 1"),
            (make(Opcode::Call, &[1]), "0000 Call 1"),
            (make(Opcode::Cast, &[CAST_INT as usize]), "0000 Cast 0"),
            (make(Opcode::JumpTruthy, &[1]), "0000 JumpTruthy 1"),
            (make(Opcode::JumpNotTruthy, &[1]), "0000 JumpNotTruthy 1"),
            (make(Opcode::Pop, &[]), "0000 Pop"),
        ];

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

        let expected = r#"0000 Constant 0
0003 Global 0
0006 Cast 1
0008 Subtract
0009 Constant 1
0012 Greater
0013 JumpNotTruthy 30
0016 Pop
0017 Global 1
0020 Constant 2
0023 Constant 3
0026 Array 2
0029 In
0030 Pop
"#;
        assert_eq!(actual, expected);
    }
}
