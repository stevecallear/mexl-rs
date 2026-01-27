use crate::token::*;

macro_rules! new_token {
    ($kind:expr, $($byte:expr),+) => {
        Token {
            token_type: $kind,
            literal: String::from_utf8(vec![$($byte),+]).expect("Invalid UTF-8 bytes"),
        }
    };
}

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
        _ => None
    }
}

pub struct Lexer<'a> {
    input: &'a[u8],
    pos: usize,
    read_pos: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let mut skip_read = false;
        
        let token = match self.ch {
            0 => Token { token_type: TokenType::EOF, literal: "".into() },
            
            b'+' => new_token!(TokenType::Plus, self.ch),
            b'-' => new_token!(TokenType::Minus, self.ch),
            b'*' => new_token!(TokenType::Asterisk, self.ch),
            b'/' => new_token!(TokenType::Slash, self.ch),
            b',' => new_token!(TokenType::Comma, self.ch),
            b'(' => new_token!(TokenType::LParen, self.ch),
            b')' => new_token!(TokenType::RParen, self.ch),
            b'[' => new_token!(TokenType::LBracket, self.ch),
            b']' => new_token!(TokenType::RBracket, self.ch),

            b'"' => {
                skip_read = true;
                self.read_string()
            }

            b'.' => {
                match self.peek_char() {
                    c if c.is_ascii_digit() => {
                        skip_read = true;
                        self.read_number()
                    },
                    _ => new_token!(TokenType::Stop, self.ch)
                }
            },
            b'=' => {
                let pc = self.ch;
                self.read_char();
                match self.ch {
                    c if c == b'=' => new_token!(TokenType::Equal, pc, self.ch),
                    _ => new_token!(TokenType::Illegal, pc, self.ch),
                }
            },
            b'!' => {
                match self.peek_char() {
                    b'=' => {
                        let ch = self.ch;
                        self.read_char();
                        new_token!(TokenType::NotEqual, ch, self.ch)
                    },
                    _ => new_token!(TokenType::Bang, self.ch),
                }
            },
            b'<' => {
                match self.peek_char() {
                    b'=' => {
                        let ch = self.ch;
                        self.read_char();
                        new_token!(TokenType::LessThanEqual, ch, self.ch)
                    },
                    _ => new_token!(TokenType::LessThan, self.ch),
                }
            },
            b'>' => {
                match self.peek_char() {
                    b'=' => {
                        let ch = self.ch;
                        self.read_char();
                        new_token!(TokenType::GreaterThanEqual, ch, self.ch)
                    },
                    _ => new_token!(TokenType::GreaterThan, self.ch),
                }
            },
            b'&' => {
                let pc = self.ch;
                self.read_char();
                match self.ch {
                    c if c == b'&' => new_token!(TokenType::And, pc, self.ch),
                    _ => new_token!(TokenType::Illegal, pc, self.ch),
                }
            },
            b'|' => {
                let pc = self.ch;
                self.read_char();
                match self.ch {
                    c if c == b'|' => new_token!(TokenType::Or, pc, self.ch),
                    _ => new_token!(TokenType::Illegal, pc, self.ch),
                }
            },

            _ => {
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

    fn peek_char(&mut self) -> u8 {
        if self.read_pos >= self.input.len() {
            return 0
        }
        self.input[self.read_pos]
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

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

        Token::new(token_type,str::from_utf8(&self.input[start..self.pos]).expect("valid utf-8").into())
    }

    fn read_word(&mut self) -> Token {
        let start = self.pos;
        loop {
            match self.ch {
                c if is_word_char(c) => {
                    self.read_char();
                },
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

    fn read_string(&mut self) -> Token {
        let start = self.pos;
        self.read_char();  // consume the opening quote

        loop {
            match self.ch {
                b'"' => {
                    self.read_char();
                    break;
                },
                b'\\' => {
                    self.read_char(); // consume the backslash
                    self.read_char();
                },
                0 => break, 
                _ => self.read_char(),
            }
        }

        Token::new(TokenType::String, str::from_utf8(&self.input[start..self.pos]).expect("valid utf-8").into())
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                b' ' | b'\t' | b'\n' | b'\r'  => self.read_char(),
                _ => break,
            }
        }
    }
}

fn is_word_start(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

fn is_word_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c == b'-' 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let tests = vec![
            ("", Token{ token_type: TokenType::EOF, literal: "".into()} ),
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
            ("\"\\\"b \\\" a\"", Token::new(TokenType::String, "\"\\\"b \\\" a\"".into())),
     
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

            ("_a1-_b", Token::new(TokenType::Ident, "_a1-_b".into()))
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
            Token::new(TokenType::String, "\"company.com\"".into())
        ];

        let mut lexer = Lexer::new(&input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            if token.token_type == TokenType::EOF { break };
            tokens.push(token);
        }

        assert_eq!(tokens, *expected)
    }
}