use crate::token::*;

macro_rules! new_token {
    ($kind:expr, $($byte:expr),+) => {
        Token {
            token_type: $kind,
            literal: String::from_utf8(vec![$($byte),+]).expect("Invalid UTF-8 bytes"),
        }
    };
}

/// Returns the TokenType for a given keyword string, if it is a keyword.
fn keyword_token_type(input: &str) -> Option<TokenType> {
    match input.to_lowercase().as_str() {
        "true" => Some(TokenType::True),
        "false" => Some(TokenType::False),
        "null" => Some(TokenType::Null),
        "and" => Some(TokenType::And),
        "or" => Some(TokenType::Or),
        "not" => Some(TokenType::Bang),
        "eq" => Some(TokenType::Equal),
        "ne" => Some(TokenType::NotEqual),
        "lt" => Some(TokenType::LessThan),
        "le" => Some(TokenType::LessThanEqual),
        "gt" => Some(TokenType::GreaterThan),
        "ge" => Some(TokenType::GreaterThanEqual),
        "in" => Some(TokenType::In),
        "sw" => Some(TokenType::StartsWith),
        "ew" => Some(TokenType::EndsWith),
        "as" => Some(TokenType::As),
        _ => None,
    }
}

/// Lexer for tokenizing input strings.
pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    read_pos: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer for the given input string.
    pub fn new(input: &'a str) -> Self {
        let mut l = Self {
            input: input.as_bytes(),
            pos: 0,
            read_pos: 0,
            ch: 0,
        };

        l.read_char();
        l
    }

    /// Returns the next token from the input.
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let mut skip_read = false;

        let token = match self.ch {
            0 => Token {
                token_type: TokenType::EOF,
                literal: "".into(),
            },

            b'"' => {
                skip_read = true;
                self.read_string()
            }

            b'.' => match self.peek_char() {
                c if c.is_ascii_digit() => {
                    skip_read = true;
                    self.read_number()
                }
                _ => new_token!(TokenType::Stop, self.ch),
            },
            b'=' => self.read_two_char_operator(TokenType::Illegal, TokenType::Equal),
            b'!' => self.read_two_char_operator_opt(TokenType::Bang, b'=', TokenType::NotEqual),
            b'<' => {
                self.read_two_char_operator_opt(TokenType::LessThan, b'=', TokenType::LessThanEqual)
            }
            b'>' => self.read_two_char_operator_opt(
                TokenType::GreaterThan,
                b'=',
                TokenType::GreaterThanEqual,
            ),
            b'&' => self.read_two_char_operator(TokenType::Illegal, TokenType::And),
            b'|' => self.read_two_char_operator(TokenType::Illegal, TokenType::Or),

            _ => {
                // Try single-character tokens first
                if let Some(token) = self.try_single_char_token(self.ch) {
                    self.read_char();
                    return token;
                }

                match self.ch {
                    c if c.is_ascii_digit() => {
                        skip_read = true;
                        self.read_number()
                    }
                    c if is_word_start(c) => {
                        skip_read = true;
                        self.read_word()
                    }
                    _ => new_token!(TokenType::Illegal, self.ch),
                }
            }
        };

        if !skip_read {
            self.read_char();
        }
        token
    }

    /// Peeks at the next character without advancing the lexer.
    fn peek_char(&mut self) -> u8 {
        if self.read_pos >= self.input.len() {
            return 0;
        }
        self.input[self.read_pos]
    }

    /// Advances the lexer to the next character.
    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    /// Try to parse a single-character token.
    /// Returns Some(Token) for recognized single-char tokens, None otherwise.
    fn try_single_char_token(&self, ch: u8) -> Option<Token> {
        match ch {
            b'+' => Some(new_token!(TokenType::Plus, ch)),
            b'-' => Some(new_token!(TokenType::Minus, ch)),
            b'*' => Some(new_token!(TokenType::Asterisk, ch)),
            b'/' => Some(new_token!(TokenType::Slash, ch)),
            b',' => Some(new_token!(TokenType::Comma, ch)),
            b'(' => Some(new_token!(TokenType::LParen, ch)),
            b')' => Some(new_token!(TokenType::RParen, ch)),
            b'[' => Some(new_token!(TokenType::LBracket, ch)),
            b']' => Some(new_token!(TokenType::RBracket, ch)),
            _ => None,
        }
    }

    /// Read a two-character operator. Assumes current char is first char of operator.
    /// If next char matches expected, returns token with both characters and advances.
    /// If next char does NOT match but is not whitespace/special, creates illegal token with both chars.
    /// Otherwise returns single-char token and does NOT advance.
    fn read_two_char_operator(&mut self, single_type: TokenType, double_type: TokenType) -> Token {
        let ch = self.ch;
        let expected = match ch {
            b'=' => b'=',
            b'&' => b'&',
            b'|' => b'|',
            _ => 0,
        };

        let next = self.peek_char();
        if next == expected {
            self.read_char();
            new_token!(double_type, ch, self.ch)
        } else if next != 0 && next != b' ' && next != b'\t' && next != b'\n' && next != b'\r' {
            // Consume the next character to create an illegal two-char token
            self.read_char();
            new_token!(TokenType::Illegal, ch, self.ch)
        } else {
            new_token!(single_type, ch)
        }
    }

    /// Read a two-character operator with optional second char.
    /// If peek matches the expected second char, consume it and return double_type token.
    /// Otherwise return single_type token.
    fn read_two_char_operator_opt(
        &mut self,
        single_type: TokenType,
        expected_second: u8,
        double_type: TokenType,
    ) -> Token {
        let ch = self.ch;
        if self.peek_char() == expected_second {
            self.read_char();
            new_token!(double_type, ch, self.ch)
        } else {
            new_token!(single_type, ch)
        }
    }

    /// Reads a number (integer or float) from the input.
    fn read_number(&mut self) -> Token {
        let start = self.pos;
        let mut token_type = TokenType::Integer;

        loop {
            match self.ch {
                b'.' if token_type == TokenType::Integer => {
                    token_type = TokenType::Float;
                    self.read_char();
                }
                c if c.is_ascii_digit() => {
                    self.read_char();
                }
                _ => break,
            }
        }

        Token::new(
            token_type,
            str::from_utf8(&self.input[start..self.pos])
                .expect("valid utf-8")
                .into(),
        )
    }

    /// Reads a word (identifier or keyword) from the input.
    fn read_word(&mut self) -> Token {
        let start = self.pos;
        loop {
            match self.ch {
                c if is_word_char(c) => {
                    self.read_char();
                }
                _ => break,
            }
        }

        let literal = str::from_utf8(&self.input[start..self.pos]).expect("valid utf-8");
        let token_type = match keyword_token_type(literal) {
            Some(t) => t,
            None => TokenType::Ident,
        };

        Token::new(token_type, literal.into())
    }

    /// Reads a string literal from the input, handling escape sequences.
    fn read_string(&mut self) -> Token {
        let start = self.pos;
        self.read_char(); // consume the opening quote

        let mut found_closing_quote = false;
        loop {
            match self.ch {
                b'"' => {
                    found_closing_quote = true;
                    self.read_char();
                    break;
                }
                b'\\' => {
                    self.read_char(); // consume the backslash
                    self.read_char();
                }
                0 => break, // EOF without closing quote
                _ => self.read_char(),
            }
        }

        let literal = str::from_utf8(&self.input[start..self.pos]).expect("valid utf-8");

        if found_closing_quote {
            Token::new(TokenType::String, literal.into())
        } else {
            Token::new(TokenType::Illegal, literal.into())
        }
    }

    /// Skips whitespace characters in the input.
    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                b' ' | b'\t' | b'\n' | b'\r' => self.read_char(),
                _ => break,
            }
        }
    }
}

/// Check if character can start an identifier (letter or underscore).
fn is_word_start(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

/// Check if character can appear in an identifier (alphanumeric, underscore, or hyphen).
/// Allows identifiers like `foo-bar` for natural language style variable names.
fn is_word_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c == b'-'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let tests = vec![
            (
                "",
                Token {
                    token_type: TokenType::EOF,
                    literal: "".into(),
                },
            ),
            ("~", new_token!(TokenType::Illegal, b'~')),
            ("+", new_token!(TokenType::Plus, b'+')),
            ("-", new_token!(TokenType::Minus, b'-')),
            ("*", new_token!(TokenType::Asterisk, b'*')),
            ("/", new_token!(TokenType::Slash, b'/')),
            (".", new_token!(TokenType::Stop, b'.')),
            (",", new_token!(TokenType::Comma, b',')),
            ("(", new_token!(TokenType::LParen, b'(')),
            (")", new_token!(TokenType::RParen, b')')),
            ("[", new_token!(TokenType::LBracket, b'[')),
            ("]", new_token!(TokenType::RBracket, b']')),
            ("!", new_token!(TokenType::Bang, b'!')),
            ("&&", new_token!(TokenType::And, b'&', b'&')),
            ("&*", new_token!(TokenType::Illegal, b'&', b'*')),
            ("||", new_token!(TokenType::Or, b'|', b'|')),
            ("|*", new_token!(TokenType::Illegal, b'|', b'*')),
            ("==", new_token!(TokenType::Equal, b'=', b'=')),
            ("=*", new_token!(TokenType::Illegal, b'=', b'*')),
            ("!=", new_token!(TokenType::NotEqual, b'!', b'=')),
            ("<", new_token!(TokenType::LessThan, b'<')),
            ("<=", new_token!(TokenType::LessThanEqual, b'<', b'=')),
            (">", new_token!(TokenType::GreaterThan, b'>')),
            (">=", new_token!(TokenType::GreaterThanEqual, b'>', b'=')),
            ("a", new_token!(TokenType::Ident, b'a')),
            ("1", new_token!(TokenType::Integer, b'1')),
            (".1", new_token!(TokenType::Float, b'.', b'1')),
            ("1.2", new_token!(TokenType::Float, b'1', b'.', b'2')),
            ("\"a\"", new_token!(TokenType::String, b'"', b'a', b'"')),
            (
                "\"\\\"b \\\" a\"",
                Token::new(TokenType::String, "\"\\\"b \\\" a\"".into()),
            ),
            ("true", Token::new(TokenType::True, "true".into())),
            ("false", Token::new(TokenType::False, "false".into())),
            ("null", Token::new(TokenType::Null, "null".into())),
            ("and", Token::new(TokenType::And, "and".into())),
            ("or", Token::new(TokenType::Or, "or".into())),
            ("not", Token::new(TokenType::Bang, "not".into())),
            ("eq", Token::new(TokenType::Equal, "eq".into())),
            ("ne", Token::new(TokenType::NotEqual, "ne".into())),
            ("lt", Token::new(TokenType::LessThan, "lt".into())),
            ("le", Token::new(TokenType::LessThanEqual, "le".into())),
            ("gt", Token::new(TokenType::GreaterThan, "gt".into())),
            ("ge", Token::new(TokenType::GreaterThanEqual, "ge".into())),
            ("in", Token::new(TokenType::In, "in".into())),
            ("sw", Token::new(TokenType::StartsWith, "sw".into())),
            ("ew", Token::new(TokenType::EndsWith, "ew".into())),
            ("as", Token::new(TokenType::As, "as".into())),
            ("_a1-_b", Token::new(TokenType::Ident, "_a1-_b".into())),
        ];

        for (input, expected) in tests.iter() {
            let string_input = input.to_string();
            let mut lexer = Lexer::new(&string_input);
            let token = lexer.next_token();
            assert_eq!(token, *expected)
        }
    }

    #[test]
    fn test_next_token_complex() {
        let input = r#"(orders + 5 <= 10) && email ew "company.com""#.to_owned();
        let expected = vec![
            new_token!(TokenType::LParen, b'('),
            Token::new(TokenType::Ident, "orders".into()),
            new_token!(TokenType::Plus, b'+'),
            new_token!(TokenType::Integer, b'5'),
            new_token!(TokenType::LessThanEqual, b'<', b'='),
            new_token!(TokenType::Integer, b'1', b'0'),
            new_token!(TokenType::RParen, b')'),
            new_token!(TokenType::And, b'&', b'&'),
            Token::new(TokenType::Ident, "email".into()),
            Token::new(TokenType::EndsWith, "ew".into()),
            Token::new(TokenType::String, "\"company.com\"".into()),
        ];

        let mut lexer = Lexer::new(&input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            if token.token_type == TokenType::EOF {
                break;
            };
            tokens.push(token);
        }

        assert_eq!(tokens, *expected)
    }

    #[test]
    fn test_single_operators() {
        let tests = vec![
            ("= ", new_token!(TokenType::Illegal, b'=')), // Single = followed by space
            ("! ", new_token!(TokenType::Bang, b'!')),    // Single ! followed by space
            ("< ", new_token!(TokenType::LessThan, b'<')), // Single < followed by space
            ("> ", new_token!(TokenType::GreaterThan, b'>')), // Single > followed by space
            ("& ", new_token!(TokenType::Illegal, b'&')), // Single & followed by space
            ("| ", new_token!(TokenType::Illegal, b'|')), // Single | followed by space
        ];

        for (input, expected) in tests.iter() {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token();
            assert_eq!(token, *expected, "Failed for input: {:?}", input);
        }
    }

    #[test]
    fn test_empty_string() {
        let input = r#""""#;
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::String);
        assert_eq!(token.literal, "\"\"");
    }

    #[test]
    fn test_string_escape_sequences() {
        let tests = vec![
            (r#""hello\"world""#, r#""hello\"world""#),
            (r#""back\\slash""#, r#""back\\slash""#),
            (r#""tab\there""#, r#""tab\there""#),
        ];

        for (input, expected_literal) in tests.iter() {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token();
            assert_eq!(token.token_type, TokenType::String);
            assert_eq!(token.literal, *expected_literal);
        }
    }

    #[test]
    fn test_unterminated_string() {
        let input = r#""unterminated"#;
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::Illegal);
        // Should contain everything from opening quote to EOF
        assert_eq!(token.literal, r#""unterminated"#);
    }

    #[test]
    fn test_multiple_dots() {
        let input = "..";
        let mut lexer = Lexer::new(&input);

        let token1 = lexer.next_token();
        assert_eq!(token1.token_type, TokenType::Stop);

        let token2 = lexer.next_token();
        assert_eq!(token2.token_type, TokenType::Stop);

        let token3 = lexer.next_token();
        assert_eq!(token3.token_type, TokenType::EOF);
    }

    #[test]
    fn test_number_with_multiple_dots() {
        let input = "1.2.3";
        let mut lexer = Lexer::new(&input);

        let token1 = lexer.next_token();
        assert_eq!(token1.token_type, TokenType::Float);
        assert_eq!(token1.literal, "1.2");

        let token2 = lexer.next_token();
        assert_eq!(token2.token_type, TokenType::Float);
        assert_eq!(token2.literal, ".3");
    }

    #[test]
    fn test_whitespace_handling() {
        let input = "  \t\n  a  \r  b  ";
        let mut lexer = Lexer::new(&input);

        let token1 = lexer.next_token();
        assert_eq!(token1.token_type, TokenType::Ident);
        assert_eq!(token1.literal, "a");

        let token2 = lexer.next_token();
        assert_eq!(token2.token_type, TokenType::Ident);
        assert_eq!(token2.literal, "b");

        let token3 = lexer.next_token();
        assert_eq!(token3.token_type, TokenType::EOF);
    }

    #[test]
    fn test_negative_number_tokens() {
        // Negative numbers should tokenize as minus operator + integer, not a single token
        let input = "-5";
        let mut lexer = Lexer::new(&input);

        let token1 = lexer.next_token();
        assert_eq!(token1.token_type, TokenType::Minus);

        let token2 = lexer.next_token();
        assert_eq!(token2.token_type, TokenType::Integer);
        assert_eq!(token2.literal, "5");
    }

    #[test]
    fn test_case_insensitive_keywords() {
        let tests = vec![
            ("TRUE", TokenType::True),
            ("True", TokenType::True),
            ("tRuE", TokenType::True),
            ("FALSE", TokenType::False),
            ("FaLsE", TokenType::False),
            ("NULL", TokenType::Null),
            ("nUlL", TokenType::Null),
            ("AND", TokenType::And),
            ("AnD", TokenType::And),
            ("OR", TokenType::Or),
            ("OR", TokenType::Or),
            ("NOT", TokenType::Bang),
            ("NoT", TokenType::Bang),
            ("EQ", TokenType::Equal),
            ("Eq", TokenType::Equal),
            ("IN", TokenType::In),
            ("In", TokenType::In),
        ];

        for (input, expected_type) in tests.iter() {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token();
            assert_eq!(
                token.token_type, *expected_type,
                "Failed for keyword: {}",
                input
            );
        }
    }

    #[test]
    fn test_hyphen_identifier_boundaries() {
        let tests = vec![
            // Hyphens at start should fail - hyphen is not word_start
            ("-foo", TokenType::Minus), // Should tokenize as minus, not identifier
            // Hyphens at end are allowed (is_word_char includes hyphen)
            ("foo-", TokenType::Ident),
            // Double hyphens in middle
            ("foo--bar", TokenType::Ident),
            // Hyphen with underscore
            ("_foo-_bar", TokenType::Ident),
        ];

        for (input, expected_first_type) in tests.iter() {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token();
            assert_eq!(
                token.token_type, *expected_first_type,
                "Failed for input: {}",
                input
            );
        }
    }
}
