use crate::MexlError;
use crate::ast::{
    CallExpression, CastExpression, Expression, Identifier, InfixExpression, MemberExpression,
    PrefixExpression,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

/// Represents the precedence levels of different operators.
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

/// Gets the precedence level for a given token type.
fn get_precedence(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::Or => Precedence::Or,
        TokenType::And => Precedence::And,
        TokenType::Equal | TokenType::NotEqual => Precedence::Equals,
        TokenType::LessThan
        | TokenType::LessThanEqual
        | TokenType::GreaterThan
        | TokenType::GreaterThanEqual
        | TokenType::In
        | TokenType::StartsWith
        | TokenType::EndsWith => Precedence::Comparison,
        TokenType::Plus | TokenType::Minus => Precedence::Sum,
        TokenType::Asterisk | TokenType::Slash => Precedence::Product,
        TokenType::LParen => Precedence::Call,
        TokenType::LBracket | TokenType::Stop => Precedence::Index,
        TokenType::As => Precedence::Cast,
        _ => Precedence::Lowest,
    }
}

/// The Parser struct that holds the lexer and current/peek tokens.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    /// Creates a new Parser instance with the given lexer.
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let curr = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            lexer,
            curr_token: curr,
            peek_token: peek,
            errors: Vec::new(),
        }
    }

    /// Parses the input and returns the resulting Expression or an error message.
    pub fn parse(&mut self) -> Result<Expression, MexlError> {
        if let Some(e) = self.parse_expression(Precedence::Lowest)
            && self.expect_peek(TokenType::Eof)
        {
            return Ok(e);
        }

        match self.errors.len() {
            0 => Err(MexlError::ParseError("unknown parser error".into())),
            _ => Err(MexlError::ParseError(self.errors.join("\n"))),
        }
    }

    /// Parses an expression based on the given precedence level.
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut exp = match self.curr_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Integer => self.parse_number::<i64, _>(Expression::IntegerLiteral),
            TokenType::Float => self.parse_number::<f64, _>(Expression::FloatLiteral),
            TokenType::String => self.parse_string_literal(),
            TokenType::True | TokenType::False => self.parse_boolean(),
            TokenType::Null => Some(Expression::Null),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::LBracket => self.parse_array_literal(),
            _ => return None,
        };

        while !self.peek_token_is(TokenType::Eof) && precedence < self.peek_precedence() {
            exp = match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::And
                | TokenType::Or
                | TokenType::Equal
                | TokenType::NotEqual
                | TokenType::LessThan
                | TokenType::LessThanEqual
                | TokenType::GreaterThan
                | TokenType::GreaterThanEqual
                | TokenType::In
                | TokenType::StartsWith
                | TokenType::EndsWith => self.parse_infix_expression(exp.unwrap()),
                TokenType::LParen => self.parse_call_expression(exp.unwrap()),
                TokenType::Stop => self.parse_member_expression(exp.unwrap()),
                TokenType::As => self.parse_cast_expression(exp.unwrap()),
                _ => return exp,
            }
        }
        exp
    }

    /// Parses an identifier expression.
    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier {
            value: self.curr_token.literal.clone(),
        }))
    }

    /// Parses a number literal (integer or float).
    fn parse_number<T, F>(&mut self, mapper: F) -> Option<Expression>
    where
        T: std::str::FromStr,
        F: Fn(T) -> Expression,
    {
        match self.curr_token.literal.parse::<T>() {
            Ok(v) => Some(mapper(v)),
            Err(_) => {
                self.errors.push(format!(
                    "could not parse number: {}",
                    self.curr_token.literal
                ));
                None
            }
        }
    }

    /// Parses a string literal.
    fn parse_string_literal(&mut self) -> Option<Expression> {
        let literal = unescape_literal(self.curr_token.literal.as_str());
        Some(Expression::StringLiteral(literal))
    }

    /// Parses a boolean literal.
    fn parse_boolean(&self) -> Option<Expression> {
        match self.curr_token.token_type {
            TokenType::True => Some(Expression::Boolean(true)),
            TokenType::False => Some(Expression::Boolean(false)),
            _ => None,
        }
    }

    /// Parses an array literal.
    fn parse_array_literal(&mut self) -> Option<Expression> {
        let args = self.parse_expression_list(TokenType::RBracket)?;
        Some(Expression::ArrayLiteral(args))
    }

    /// Parses a list of expressions until the specified closing token type is encountered.
    fn parse_expression_list(&mut self, close_token_type: TokenType) -> Option<Vec<Expression>> {
        let mut list = Vec::new();

        if self.peek_token_is(close_token_type) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        // If the first token after '(' is a comma, that's an error: expected an expression
        if self.curr_token.token_type == TokenType::Comma {
            let msg = format!(
                "invalid expression list: got {}, expected expression",
                self.curr_token.token_type.as_str()
            );
            self.errors.push(msg);
            return None;
        }

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token(); // consume the comma
            self.next_token();
            // If we hit the closing token right after a comma, it's an error: expected an expression
            if self.curr_token.token_type == close_token_type {
                let msg = format!(
                    "invalid expression list: got {}, expected expression",
                    self.curr_token.token_type.as_str()
                );
                self.errors.push(msg);
                return None;
            }
            let exp = self.parse_expression(Precedence::Lowest)?;
            list.push(exp);
        }

        if !self.expect_peek(close_token_type) {
            return None;
        }

        Some(list)
    }

    /// Parses a prefix expression.
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

    /// Parses an infix expression.
    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let token = self.curr_token.clone();
        let operator = token.literal.to_lowercase();
        let precedence = self.curr_precedence();
        self.next_token();

        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix(Box::new(InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })))
    }

    /// Parses a grouped expression enclosed in parentheses.
    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        Some(exp)
    }

    /// Parses a member access expression.
    fn parse_member_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let member = Identifier {
            value: self.curr_token.literal.clone(),
        };

        Some(Expression::Member(Box::new(MemberExpression {
            left: Box::new(left),
            member,
        })))
    }

    /// Parses a type cast expression.
    fn parse_cast_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let target_type = Identifier {
            value: self.curr_token.literal.clone(),
        };

        Some(Expression::Cast(Box::new(CastExpression {
            left: Box::new(left),
            target_type,
        })))
    }

    /// Parses a function call expression.
    fn parse_call_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let args = self.parse_expression_list(TokenType::RParen)?;
        Some(Expression::Call(Box::new(CallExpression {
            function: Box::new(left),
            arguments: args,
        })))
    }

    /// Advances to the next token in the lexer.
    fn next_token(&mut self) {
        self.curr_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token())
    }

    /// Checks if the peek token matches the given token type.
    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    /// Expects the peek token to match the given token type and advances if it does.
    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token.token_type == token_type {
            self.next_token();
            true
        } else {
            self.errors.push(format!(
                "invalid expression: got {}, expected {}",
                self.peek_token.token_type.as_str(),
                token_type.as_str()
            ));
            false
        }
    }

    /// Gets the precedence of the current token.
    fn curr_precedence(&self) -> Precedence {
        get_precedence(&self.curr_token.token_type)
    }

    /// Gets the precedence of the peek token.
    fn peek_precedence(&self) -> Precedence {
        get_precedence(&self.peek_token.token_type)
    }
}

/// Unescapes a string literal by handling escape sequences.
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

    fn assert_parser_output(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let input_string = input.to_string();
            let mut parser = Parser::new(Lexer::new(&input_string));
            let exp = parser.parse().unwrap();
            assert_eq!(format!("{}", exp), expected);
        }
    }

    #[test]
    fn test_whitespace() {
        let tests = vec![
            ("   42   ", "42"),
            ("\n\ttrue\n", "true"),
            ("  \"abc\"  ", "\"abc\""),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_basic_arithmetic_precedence() {
        let tests = vec![("1 + 2", "(1 + 2)"), ("x * (y + 2)", "(x * (y + 2))")];
        assert_parser_output(tests);
    }

    #[test]
    fn test_comparison_operators() {
        let tests = vec![("x != y", "(x != y)"), ("x != null", "(x != null)")];
        assert_parser_output(tests);
    }

    #[test]
    fn test_in_operator() {
        let tests = vec![
            ("1 in x", "(1 in x)"),
            ("x in [1, \"a\"]", "(x in [1, \"a\"])"),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_string_operators() {
        let tests = vec![
            (r#""hello" sw "h" + "e""#, r#"("hello" sw ("h" + "e"))"#),
            ("x ne y and a lt b", "((x ne y) and (a lt b))"),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![("[1]", "[1]"), ("[]", "[]")];
        assert_parser_output(tests);
    }

    #[test]
    fn test_parser_errors() {
        let tests: Vec<&'static str> = vec![
            "",
            "fn(1, 2]",
            "(1 + 2]",
            "true as 1",
            "m.true",
            "fn(1, )",
            "fn(,1)",
            "true true",
        ];

        for input in tests {
            let input_string = input.to_string();
            let lexer = Lexer::new(&input_string);
            let mut parser = Parser::new(lexer);
            let result = parser.parse();
            assert!(matches!(result, Err(MexlError::ParseError(_))));
        }
    }

    #[test]
    fn test_call_expressions() {
        let tests = vec![
            ("fn()", "fn()"),
            ("fn(1+2, x)", "fn((1 + 2), x)"),
            ("a(b())", "a(b())"),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_member_call_chaining() {
        let tests = vec![
            ("a.b()", "(a.b)()"),
            ("a().b", "(a().b)"),
            ("a().b().c(1)", "((a().b)().c)(1)"),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_associativity_and_unary() {
        let tests = vec![
            ("a - b - c", "((a - b) - c)"),
            ("- -a", "(-(-a))"),
            ("not not a", "(not (not a))"),
            ("-fn()", "(-fn())"),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_cast_precedence_and_negative_float() {
        let tests = vec![("x as float * y", "((x as float) * y)"), ("-1.5", "(-1.5)")];
        assert_parser_output(tests);
    }

    #[test]
    fn test_calls_with_array_and_nested() {
        let tests = vec![
            ("f([1, 2], g(3))", "f([1, 2], g(3))"),
            ("g([ [1], 2 ])", "g([[1], 2])"),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_left_associativity_multiply_divide() {
        let tests = vec![
            ("a / b / c", "((a / b) / c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a / b * c", "((a / b) * c)"),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_complex_unary_and_operators() {
        let tests = vec![
            ("!a && b", "((!a) && b)"),
            ("!a || b", "((!a) || b)"),
            ("!(a && b)", "(!(a && b))"),
            ("- -5 + 3", "((-(-5)) + 3)"),
            ("not a and b", "((not a) and b)"),
        ];
        assert_parser_output(tests);
    }

    #[test]
    fn test_nested_calls_and_members() {
        let tests = vec![
            ("a(b(c(d)))", "a(b(c(d)))"),
            ("a.b.c.d", "(((a.b).c).d)"),
            ("a(1).b(2).c", "((a(1).b)(2).c)"),
            ("a.b(1, 2).c(3)", "((a.b)(1, 2).c)(3)"),
        ];
        assert_parser_output(tests);
    }
}
