use crate::ast::{CallExpression, CastExpression, Expression, Identifier, InfixExpression, MemberExpression, PrefixExpression};
use crate::token::{Token, TokenType};
use crate::lexer::{Lexer};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Or,
    And,
    Equals,
    Comparison,
    Sum,
    Product,
    Prefix,
    Cast,
    Call,
    Index,
}

fn get_precedence(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::Or => Precedence::Or,
        TokenType::And => Precedence::And,
        TokenType::Equal | TokenType::NotEqual => Precedence::Equals,
        TokenType::LessThan | TokenType::LessThanEqual | TokenType::GreaterThan | TokenType::GreaterThanEqual | 
        TokenType::In | TokenType::StartsWith | TokenType::EndsWith => Precedence::Comparison,
        TokenType::Plus | TokenType::Minus => Precedence::Sum,
        TokenType::Asterisk | TokenType::Slash => Precedence::Product,
        TokenType::LParen => Precedence::Call,
        TokenType::LBracket | TokenType::Stop => Precedence::Index,
        TokenType::As => Precedence::Cast,
        _ => Precedence::Lowest,
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let curr = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            lexer: lexer,
            curr_token: curr,
            peek_token: peek,
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Expression, String> {
        match self.parse_expression(Precedence::Lowest) {
            Some(e) => Ok(e),
            None => {
                match self.errors.len() {
                    0 => Err("unknown parser error".into()),
                    _ => Err(self.errors.join("\n")),
                }
            }
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {    
        let mut exp = match self.curr_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Integer => self.parse_integer_literal(),
            TokenType::Float => self.parse_float_literal(),
            TokenType::String => self.parse_string_literal(),
            TokenType::True | TokenType::False => self.parse_boolean(),
            TokenType::Null => Some(Expression::Null),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::LBracket => self.parse_array_literal(),
            _ => return None,
        };

        while !self.peek_token_is(TokenType::EOF) && precedence < self.peek_precedence() {
            match self.peek_token.token_type {
                TokenType::Plus | TokenType::Minus | TokenType::Slash | TokenType::Asterisk |
                TokenType::And | TokenType::Or |
                TokenType::Equal | TokenType::NotEqual | 
                TokenType::LessThan | TokenType::LessThanEqual | TokenType::GreaterThan | TokenType::GreaterThanEqual | 
                TokenType::In | TokenType::StartsWith | TokenType::EndsWith => {
                    exp = self.parse_infix_expression(exp.unwrap());
                },
                TokenType::LParen => {                    
                    exp = self.parse_call_expression(exp.unwrap());
                },
                TokenType::Stop => {
                    exp = self.parse_member_expression(exp.unwrap());
                },
                TokenType::As => {
                    exp = self.parse_cast_expression(exp.unwrap());
                }
                _ => return exp,
            }
        }
        exp
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier{
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        match self.curr_token.literal.parse::<i64>() {
            Ok(i) => Some(Expression::IntegerLiteral(i)),
            Err(_) => {
                let msg = format!("could not parse integer: {}", self.curr_token.literal);
                self.errors.push(msg);
                None
            }
        }
    }

    fn parse_float_literal(&mut self) -> Option<Expression> {
        match self.curr_token.literal.parse::<f64>() {
            Ok(f) => Some(Expression::FloatLiteral(f)),
            Err(_) => {
                let msg = format!("could not parse float: {}", self.curr_token.literal);
                self.errors.push(msg);
                None
            }
        }
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        let literal = unescape_literal(self.curr_token.literal.as_str());
        Some(Expression::StringLiteral(literal))
    }

    fn parse_boolean(&self) -> Option<Expression> {
        match self.curr_token.token_type {
            TokenType::True => Some(Expression::Boolean(true)),
            TokenType::False => Some(Expression::Boolean(false)),
            _ => None
        }
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let args = self.parse_expression_list(TokenType::RBracket)?;
        Some(Expression::ArrayLiteral(args))
    }

    fn parse_expression_list(&mut self, close_token_type: TokenType) -> Option<Vec<Expression>> {
        let mut list = Vec::new();

        if self.peek_token_is(close_token_type) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token(); // consume the comma
            self.next_token();
            let exp = self.parse_expression(Precedence::Lowest)?;            
            list.push(exp);
        }

        if !self.expect_peek(close_token_type) {
            let msg = format!("invalid expression list: got {}, expected {}", self.peek_token.literal, close_token_type.as_str());
            self.errors.push(msg);
            return None;
        }

        Some(list)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone();
        let operator = token.literal.to_lowercase();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(Box::new(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        })))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();            

        let token = self.curr_token.clone();
        let operator = token.literal.to_lowercase();

        let precedence = self.curr_precedence();
        self.next_token();

        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix(Box::new(InfixExpression{
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(TokenType::RParen) {
            let msg = format!("invalid grouped expression: got {}, expected {}", self.peek_token.literal, TokenType::RParen.as_str());
            self.errors.push(msg);
            return None;
        }

        exp
    }

    fn parse_member_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();
        
        if !self.expect_peek(TokenType::Ident) {
            let msg = format!("invalid member expression: got {}, expected {}", self.peek_token.literal, TokenType::Ident.as_str());
            self.errors.push(msg);
            return None;
        }

        let member = Identifier{
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        Some(Expression::Member(Box::new(MemberExpression {
            left: Box::new(left),
            member,
        })))
    }

    fn parse_cast_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        if !self.expect_peek(TokenType::Ident) {
            let msg = format!("invalid cast expression: got {}, expected {}", self.peek_token.literal, TokenType::Ident.as_str());
            self.errors.push(msg);
            return None;
        }

        let target_type = Identifier{
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        Some(Expression::Cast(Box::new(CastExpression {
            left: Box::new(left),
            target_type,
        })))
    }

    fn parse_call_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let args = self.parse_expression_list(TokenType::RParen)?;
        Some(Expression::Call(Box::new(CallExpression {
            function: Box::new(left),
            arguments: args,
        })))
    }

    fn next_token(&mut self) {
        self.curr_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token())
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token.token_type == token_type {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn curr_precedence(&self) -> Precedence {
        get_precedence(&self.curr_token.token_type)
    }

    fn peek_precedence(&self) -> Precedence {
        get_precedence(&self.peek_token.token_type)
    }    
}


fn unescape_literal(raw: &str) -> String {
    if raw.len() < 2 {
        return String::new();
    }

    let content = &raw[1..raw.len() - 1];

    let mut output = String::with_capacity(content.len());
    let mut chars = content.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => output.push('\n'), 
                Some('r') => output.push('\r'),
                Some('t') => output.push('\t'),
                Some('"') => output.push('"'),
                Some('\\') => output.push('\\'),
                Some(other) => output.push(other),
                None => output.push('\\'),
            }
        } else {
            output.push(c);
        }
    }

    output
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_precedence() {
        let tests = vec![
            ("1 + 2", "(1 + 2)"),
            ("2.5 - -.5", "(2.5 - (-0.5))"),
            ("-1 * (4 / 2)", "((-1) * (4 / 2))"),
            ("\"a\" + \"b\"", "(\"a\" + \"b\")"),
            ("x / 2 - 1", "((x / 2) - 1)"),
            ("x * (y + 2)", ("(x * (y + 2))")),
            ("x != y", "(x != y)"),
            ("x == true && y != false", "((x == true) && (y != false))"),
            ("x != null", "(x != null)"),
            ("x > y && y < z", ("((x > y) && (y < z))")),
            ("x > 1 || y < 3 && z >= 4", ("((x > 1) || ((y < 3) && (z >= 4)))")),
            ("x ne y and a lt b", "((x ne y) and (a lt b))"),
            ("1 in x", "(1 in x)"),
            ("[1]", "[1]"),
            ("[]", "[]"),
            ("x in [1, \"a\"]", "(x in [1, \"a\"])"),
            (r#""hello" sw "h" + "e""#, r#"("hello" sw ("h" + "e"))"#),
            ("x.y", "(x.y)"),
            ("x.y + 3", "((x.y) + 3)"),
            ("x as float", "(x as float)"),
            ("x as int lt 3", "((x as int) lt 3)"),
        ];

        for (input, expected) in tests {
            let input_string = input.to_string();
            let mut parser = Parser::new(Lexer::new(&input_string));
            
            let exp = parser.parse().unwrap();
            assert_eq!(format!("{}", exp), expected);
        }
    }

    #[test]
    fn test_parser_errors() {
        let tests: Vec<(&'static str, &'static str)> = vec![
            ("fn(1, 2]", "invalid expression list: got ], expected )"),
            ("(1 + 2]", "invalid grouped expression: got ], expected )"),
            ("true as 1", "invalid cast expression: got 1, expected IDENT"),
            ("m.true", "invalid member expression: got true, expected IDENT"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let result = parser.parse();
            assert!(result.is_err(), "expected parser error, got {:?}", result.unwrap());
            assert_eq!(result.unwrap_err(), expected);
        }
    }
}