use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    EOF,
    Ident,
    Integer,
    Float,
    String,
    True,
    False,
    Null,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Stop,
	Comma,
    LParen,
	RParen,
	LBracket,
	RBracket,
	Bang,
    And,
	Or,
	Equal,
	NotEqual,
	LessThan,
	LessThanEqual,
	GreaterThan,
	GreaterThanEqual,
    In,
    StartsWith,
    EndsWith,
    As,
}

impl TokenType {
    pub fn as_str(self) -> &'static str {
        match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::Ident => "IDENT",
            TokenType::Integer => "INTEGER",
            TokenType::Float => "FLOAT",
            TokenType::String => "STRING",
            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
            TokenType::Null => "NULL",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::Stop => ".",
            TokenType::Comma => ",",
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBracket => "[",
            TokenType::RBracket => "]",
            TokenType::Bang => "!",
            TokenType::And => "&&",
            TokenType::Or => "||",
            TokenType::Equal => "==",
            TokenType::NotEqual => "!=",
            TokenType::LessThan => "<",
            TokenType::LessThanEqual => "<=",
            TokenType::GreaterThan => ">",
            TokenType::GreaterThanEqual => ">=",
            TokenType::In => "IN",
            TokenType::StartsWith => "SW",
            TokenType::EndsWith => "EW",
            TokenType::As => "AS",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self { token_type, literal }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}: {}", self.token_type.as_str(), self.literal)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_type_as_str() {
        let tests: Vec<(TokenType, &'static str)> = vec![
            (TokenType::Illegal, "ILLEGAL"),
            (TokenType::EOF, "EOF"),
            (TokenType::Ident, "IDENT"),
            (TokenType::Integer, "INTEGER"),
            (TokenType::Float, "FLOAT"),
            (TokenType::String, "STRING"),
            (TokenType::True, "TRUE"),
            (TokenType::False, "FALSE"),
            (TokenType::Null, "NULL"),
            (TokenType::Plus, "+"),
            (TokenType::Minus, "-"),
            (TokenType::Asterisk, "*"),
            (TokenType::Slash, "/"),
            (TokenType::Stop, "."),
            (TokenType::Comma, ","),
            (TokenType::LParen, "("),
            (TokenType::RParen, ")"),
            (TokenType::LBracket, "["),
            (TokenType::RBracket, "]"),
            (TokenType::Bang, "!"),
            (TokenType::And, "&&"),
            (TokenType::Or, "||"),
            (TokenType::Equal, "=="),
            (TokenType::NotEqual, "!="),
            (TokenType::LessThan, "<"),
            (TokenType::LessThanEqual, "<="),
            (TokenType::GreaterThan, ">"),
            (TokenType::GreaterThanEqual, ">="),
            (TokenType::In, "IN"),
            (TokenType::StartsWith, "SW"),
            (TokenType::EndsWith, "EW"),
            (TokenType::As, "AS"),
        ];

        for (input, expected) in tests {
            let actual = input.as_str();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_token_new() {
        let token = Token::new(TokenType::Ident, "x".into());
        let expected = Token { token_type: TokenType::Ident, literal: "x".into() };
        assert_eq!(token, expected);
    }

    #[test]
    fn test_token_fmt() {
        let token = Token::new(TokenType::Ident, "x".into());
        let actual = format!("{}", token);
        let expected = "IDENT: x".to_owned();
        assert_eq!(actual, expected);
    }
}