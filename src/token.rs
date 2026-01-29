use std::fmt;

/// Represents the different types of tokens in the language.
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
    /// Returns the string representation of the token type.
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
            TokenType::Plus => "PLUS",
            TokenType::Minus => "MINUS",
            TokenType::Asterisk => "ASTERISK",
            TokenType::Slash => "SLASH",
            TokenType::Stop => "STOP",
            TokenType::Comma => "COMMA",
            TokenType::LParen => "LPAREN",
            TokenType::RParen => "RPAREN",
            TokenType::LBracket => "LBRACKET",
            TokenType::RBracket => "RBRACKET",
            TokenType::Bang => "BANG",
            TokenType::And => "AND",
            TokenType::Or => "OR",
            TokenType::Equal => "EQUAL",
            TokenType::NotEqual => "NOTEQUAL",
            TokenType::LessThan => "LESSTHAN",
            TokenType::LessThanEqual => "LESSTHANEQUAL",
            TokenType::GreaterThan => "GREATERTHAN",
            TokenType::GreaterThanEqual => "GREATERTHANEQUAL",
            TokenType::In => "IN",
            TokenType::StartsWith => "STARTSWITH",
            TokenType::EndsWith => "ENDSWITH",
            TokenType::As => "AS",
        }
    }
}

/// Represents a token with its type and literal value.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    /// Creates a new Token instance.
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self { token_type, literal }
    }
}

impl fmt::Display for Token {
    /// Formats the token as a string.
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
            (TokenType::Plus, "PLUS"),
            (TokenType::Minus, "MINUS"),
            (TokenType::Asterisk, "ASTERISK"),
            (TokenType::Slash, "SLASH"),
            (TokenType::Stop, "STOP"),
            (TokenType::Comma, "COMMA"),
            (TokenType::LParen, "LPAREN"),
            (TokenType::RParen, "RPAREN"),
            (TokenType::LBracket, "LBRACKET"),
            (TokenType::RBracket, "RBRACKET"),
            (TokenType::Bang, "BANG"),
            (TokenType::And, "AND"),
            (TokenType::Or, "OR"),
            (TokenType::Equal, "EQUAL"),
            (TokenType::NotEqual, "NOTEQUAL"),
            (TokenType::LessThan, "LESSTHAN"),
            (TokenType::LessThanEqual, "LESSTHANEQUAL"),
            (TokenType::GreaterThan, "GREATERTHAN"),
            (TokenType::GreaterThanEqual, "GREATERTHANEQUAL"),
            (TokenType::In, "IN"),
            (TokenType::StartsWith, "STARTSWITH"),
            (TokenType::EndsWith, "ENDSWITH"),
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
    fn test_token_display() {
        let tests: Vec<(TokenType, &str, &str)> = vec![
            (TokenType::Ident, "x", "IDENT: x"),
            (TokenType::Integer, "42", "INTEGER: 42"),
            (TokenType::String, "hello", "STRING: hello"),
            (TokenType::Plus, "+", "PLUS: +"),
            (TokenType::Minus, "-", "MINUS: -"),
            (TokenType::Asterisk, "*", "ASTERISK: *"),
            (TokenType::Slash, "/", "SLASH: /"),
            (TokenType::Bang, "!", "BANG: !"),
            (TokenType::And, "&&", "AND: &&"),
            (TokenType::Or, "||", "OR: ||"),
            (TokenType::Equal, "==", "EQUAL: =="),
            (TokenType::NotEqual, "!=", "NOTEQUAL: !="),
            (TokenType::LessThan, "<", "LESSTHAN: <"),
            (TokenType::LessThanEqual, "<=", "LESSTHANEQUAL: <="),
            (TokenType::GreaterThan, ">", "GREATERTHAN: >"),
            (TokenType::GreaterThanEqual, ">=", "GREATERTHANEQUAL: >="),
            (TokenType::Stop, ".", "STOP: ."),
            (TokenType::Comma, ",", "COMMA: ,"),
            (TokenType::LParen, "(", "LPAREN: ("),
            (TokenType::RParen, ")", "RPAREN: )"),
        ];

        for (token_type, literal, expected) in tests {
            let token = Token::new(token_type, literal.into());
            let formatted = format!("{}", token);
            assert_eq!(formatted, expected);
        }
    }

    #[test]
    fn test_token_with_empty_literal() {
        let token = Token::new(TokenType::String, String::new());
        assert_eq!(token.literal, "");
        assert_eq!(format!("{}", token), "STRING: ");
    }

    #[test]
    fn test_token_with_special_characters() {
        let tests = vec![
            (TokenType::String, r#""hello""#),
            (TokenType::Integer, "12345"),
            (TokenType::Float, "3.14"),
        ];

        for (token_type, literal) in tests {
            let token = Token::new(token_type, literal.into());
            assert_eq!(token.literal, literal);
            let formatted = format!("{}", token);
            assert!(formatted.contains(literal));
        }
    }

    #[test]
    fn test_token_equality() {
        let token1 = Token::new(TokenType::Ident, "x".into());
        let token2 = Token::new(TokenType::Ident, "x".into());
        let token3 = Token::new(TokenType::Ident, "y".into());
        let token4 = Token::new(TokenType::Integer, "x".into());

        assert_eq!(token1, token2);
        assert_ne!(token1, token3); // Different literal
        assert_ne!(token1, token4); // Different token type
    }

}