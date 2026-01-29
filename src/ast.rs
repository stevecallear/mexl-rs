use std::fmt;

use crate::token::Token;

/// Represents an expression in the abstract syntax tree (AST).
#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    ArrayLiteral(Vec<Expression>),
    Boolean(bool),
    Null,
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Call(Box<CallExpression>),
    Member(Box<MemberExpression>),
    Cast(Box<CastExpression>),
}

impl fmt::Display for Expression {
    /// Formats the expression as a string.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Expression::Identifier(v) => write!(f, "{}", v),
            Expression::IntegerLiteral(v) => write!(f, "{}", v),
            Expression::FloatLiteral(v) => write!(f, "{}", v),
            Expression::StringLiteral(v) => write!(f, "\"{}\"", v),
            Expression::ArrayLiteral(v) => {
                write!(f, "[")?;
                for (i, obj) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", obj)?;
                }
                write!(f, "]")
            }
            Expression::Boolean(v) => write!(f, "{}", v),
            Expression::Null => write!(f, "null"),
            Expression::Prefix(v) => {
                match v.operator.len() {
                    // ensure readability for longer operators
                    l if l <= 1 => write!(f, "({}{})", v.operator, v.right),
                    _ => write!(f, "({} {})", v.operator, v.right),
                }
            }
            Expression::Infix(v) => write!(f, "({} {} {})", v.left, v.operator, v.right),
            Expression::Call(v) => {
                write!(f, "{}(", v.function)?;
                for (i, obj) in v.arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", obj)?;
                }
                write!(f, ")")
            }
            Expression::Member(v) => write!(f, "({}.{})", v.left, v.member),
            Expression::Cast(v) => write!(f, "({} as {})", v.left, v.target_type),
        }
    }
}

/// Represents an identifier in the AST.
#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
}

impl fmt::Display for Identifier {
    /// Formats the identifier as a string.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.value)
    }
}

/// Represents a prefix expression in the AST.
#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

/// Represents an infix expression in the AST.
#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

/// Represents a function call expression in the AST.
#[derive(Clone, Debug)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

/// Represents a member access expression in the AST.
#[derive(Clone, Debug)]
pub struct MemberExpression {
    pub left: Box<Expression>,
    pub member: Identifier,
}

/// Represents a type cast expression in the AST.
#[derive(Clone, Debug)]
pub struct CastExpression {
    pub left: Box<Expression>,
    pub target_type: Identifier,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_expression_fmt() {
        let tests: Vec<(Expression, &str)> = vec![
            (Expression::IntegerLiteral(1), "1"),
            (Expression::FloatLiteral(1.5), "1.5"),
            (Expression::StringLiteral("abc".into()), r#""abc""#),
            (
                Expression::ArrayLiteral(vec![
                    Expression::IntegerLiteral(1),
                    Expression::Prefix(Box::new(PrefixExpression {
                        token: Token::new(TokenType::Minus, "-".into()),
                        operator: "-".into(),
                        right: Box::new(Expression::FloatLiteral(1.5)),
                    })),
                    Expression::StringLiteral("abc".into()),
                ]),
                r#"[1, (-1.5), "abc"]"#,
            ),
            (Expression::Boolean(true), "true"),
            (Expression::Boolean(false), "false"),
            (Expression::Null, "null"),
            (
                Expression::Prefix(Box::new(PrefixExpression {
                    token: Token::new(TokenType::Minus, "-".into()),
                    operator: "-".into(),
                    right: Box::new(Expression::IntegerLiteral(1)),
                })),
                "(-1)",
            ),
            (
                Expression::Prefix(Box::new(PrefixExpression {
                    token: Token::new(TokenType::Bang, "not".into()),
                    operator: "not".into(),
                    right: Box::new(Expression::Boolean(true)),
                })),
                "(not true)",
            ),
            (
                Expression::Infix(Box::new(InfixExpression {
                    token: Token::new(TokenType::Plus, "+".into()),
                    left: Box::new(Expression::IntegerLiteral(1)),
                    operator: "+".into(),
                    right: Box::new(Expression::IntegerLiteral(2)),
                })),
                "(1 + 2)",
            ),
            (
                Expression::Member(Box::new(MemberExpression {
                    left: Box::new(Expression::Identifier(Identifier { value: "x".into() })),
                    member: Identifier { value: "y".into() },
                })),
                "(x.y)",
            ),
            (
                Expression::Cast(Box::new(CastExpression {
                    left: Box::new(Expression::Identifier(Identifier { value: "x".into() })),
                    target_type: Identifier {
                        value: "float".into(),
                    },
                })),
                "(x as float)",
            ),
            (
                Expression::Call(Box::new(CallExpression {
                    function: Box::new(Expression::Identifier(Identifier { value: "fn".into() })),
                    arguments: vec![Expression::IntegerLiteral(1), Expression::IntegerLiteral(2)],
                })),
                "fn(1, 2)",
            ),
        ];

        for (input, expected) in tests {
            let actual = format!("{}", input);
            assert_eq!(actual, expected);
        }
    }
}
