use std::mem;

use anyhow::Result;

use crate::{ast::*, lexer::Lexer, token::Token};

type PrefixFn = fn(parser: &mut Parser) -> Result<Expression, ParserError>;
type InfixFn = fn(parser: &mut Parser, left: Expression) -> Result<Expression, ParserError>;

#[derive(PartialEq, PartialOrd, Debug)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    // Call,
}

impl Precedence {
    fn token_precedence(token: &Token) -> Precedence {
        match token {
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    errors: Vec<ParserError>,
    curr_token: Token,
    peek_token: Token,
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedIdentifierToken(Token),
    ExpectedAssign(Token),
    ExpectedReturnValue(Token),
    NoPrefixFun(Token),
    IdentParse,
    IntLitParse,
    ExpectedRparen(Token),
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            errors: vec![],
            curr_token: Token::Illegal,
            peek_token: Token::Illegal,
        };

        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.curr_token = mem::replace(&mut self.peek_token, self.lexer.next_token().unwrap())
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = vec![];

        while !self.curr_token_is(&Token::Eof) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.next_token()
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let name;
        if let Token::Ident(ident) = self.peek_token.clone() {
            self.next_token();
            name = ident;
        } else {
            return Err(ParserError::ExpectedIdentifierToken(
                self.peek_token.clone(),
            ));
        }

        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)?;

        while !self.curr_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let(LetStatement {
            name,
            value: Expression::None,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        while self.curr_token != Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return(ReturnStatement { value }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token()
        }

        Ok(Statement::Expression(ExpressionStatement { expression }))
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression, ParserError> {
        let mut left_exp;

        if let Some(f) = self.prefix_fn() {
            left_exp = f(self)?;
        } else {
            return Err(ParserError::NoPrefixFun(self.curr_token.clone()));
        }

        while !self.curr_token_is(&Token::Semicolon) && prec < self.peek_precedence() {
            match self.infix_fn() {
                Some(f) => {
                    self.next_token();
                    left_exp = f(self, left_exp)?;
                }
                None => return Ok(left_exp),
            }
        }

        Ok(left_exp)
    }

    fn prefix_fn(&mut self) -> Option<PrefixFn> {
        match self.curr_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer_literal),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            Token::True | Token::False => Some(Parser::parse_boolean),
            Token::Lparen => Some(Parser::parse_grouped_expression),
            _ => None,
        }
    }

    fn infix_fn(&mut self) -> Option<InfixFn> {
        match self.peek_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Eq
            | Token::NotEq
            | Token::Lt
            | Token::Gt => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        if let Token::Ident(ref name) = self.curr_token {
            return Ok(Expression::Identifier(name.to_string()));
        }

        Err(ParserError::IdentParse)
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParserError> {
        if let Token::Int(ref val) = self.curr_token {
            return Ok(Expression::Integer(*val));
        }

        Err(ParserError::IntLitParse)
    }

    fn parse_boolean(&mut self) -> Result<Expression, ParserError> {
        match self.curr_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => panic!("couldn't parse {:?} into a booolean", self.curr_token),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let operator = self.curr_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(Box::new(PrefixExpression {
            operator,
            right,
        })))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let operator = self.curr_token.clone();
        let precedence = self.curr_precedence();

        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(Box::new(InfixExpression {
            operator,
            right_value: right,
            left_value: left,
        })))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        self.expect_peek(Token::Rparen, ParserError::ExpectedRparen)?;

        exp
    }

    fn curr_token_is(&self, token: &Token) -> bool {
        token == &self.curr_token
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        token == &self.peek_token
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::token_precedence(&self.peek_token)
    }

    fn curr_precedence(&self) -> Precedence {
        Precedence::token_precedence(&self.curr_token)
    }

    fn expect_peek(
        &mut self,
        token: Token,
        expected: fn(Token) -> ParserError,
    ) -> Result<(), ParserError> {
        match self.peek_token_is(&token) {
            true => {
                self.next_token();
                Ok(())
            }
            false => Err(expected(self.peek_token.clone())),
        }
    }

    #[cfg(test)]
    fn errors(&self) -> &[ParserError] {
        &self.errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statement() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
        let l = Lexer::new(input.into());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        let tests = vec!["x", "y", "foobar"];

        let mut iter = program.statements.iter();

        for t in tests {
            match iter.next().unwrap() {
                Statement::Let(ref l) => {
                    assert_eq!(l.name, t)
                }
                _ => panic!("unkown node"),
            }
        }
    }

    #[test]
    fn return_statement() {
        let input = "  return 5;
        return 10;
        return 993322;";

        let lexer = Lexer::new(input.into());
        let mut p = Parser::new(lexer);
        let program = p.parse_program();

        check_parser_errors(&p);

        assert_eq!(
            program.statements,
            vec![
                Statement::Return(ReturnStatement {
                    value: Expression::Integer(5)
                }),
                Statement::Return(ReturnStatement {
                    value: Expression::Integer(10)
                }),
                Statement::Return(ReturnStatement {
                    value: Expression::Integer(993322)
                })
            ]
        );
    }
    #[test]
    fn identifiers_expression() {
        let input = "foobar;";
        let l = Lexer::new(input.into());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        let exp = match program.statements.first().unwrap() {
            Statement::Expression(stmt) => &stmt.expression,
            stmt => panic!("{:?} isn't an expression", stmt),
        };

        test_indentifier(exp, "foobar");
    }

    #[test]
    fn integer_literals() {
        let input = "5;";
        let l = Lexer::new(input.into());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        let exp = match program.statements.first().unwrap() {
            Statement::Expression(stmt) => &stmt.expression,
            stmt => panic!("{:?} isn't an expression", stmt),
        };

        match exp {
            Expression::Integer(int) => {
                assert_eq!(*int, 5, "expected {} but got {}", 5, int)
            }
            _ => panic!("expected idenifier but got {:?}", exp),
        }
    }

    #[test]
    fn prefix_expression() {
        struct Test<'a> {
            input: &'a str,
            operator: Token,
            value: i64,
        }

        let tests = vec![
            Test {
                input: "!5",
                operator: Token::Bang,
                value: 5,
            },
            Test {
                input: "-15",
                operator: Token::Minus,
                value: 15,
            },
        ];

        for t in tests {
            let l = Lexer::new(t.input.into());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);

            let exp = match program.statements.first().unwrap() {
                Statement::Expression(stmt) => &stmt.expression,
                stmt => panic!("{:?} isn't an expression", stmt),
            };

            match exp {
                Expression::Prefix(prefix) => {
                    assert_eq!(
                        t.operator, prefix.operator,
                        "expected {} operator but got {}",
                        t.operator, prefix.operator
                    );
                    test_integer_literal(&prefix.right, t.value);
                }
                _ => panic!("expected prefix operator but got {:?}", exp),
            }
        }
    }

    #[test]
    fn infix_expressions() {
        struct Test<'a> {
            input: &'a str,
            left_value: i64,
            operator: Token,
            right_value: i64,
        }

        let tests = vec![
            Test {
                input: "5 + 5;",
                left_value: 5,
                operator: Token::Plus,
                right_value: 5,
            },
            Test {
                input: "5 - 5;",
                left_value: 5,
                operator: Token::Minus,
                right_value: 5,
            },
            Test {
                input: "5 * 5;",
                left_value: 5,
                operator: Token::Asterisk,
                right_value: 5,
            },
            Test {
                input: "5 / 5;",
                left_value: 5,
                operator: Token::Slash,
                right_value: 5,
            },
            Test {
                input: "5 > 5;",
                left_value: 5,
                operator: Token::Gt,
                right_value: 5,
            },
            Test {
                input: "5 < 5;",
                left_value: 5,
                operator: Token::Lt,
                right_value: 5,
            },
            Test {
                input: "5 == 5;",
                left_value: 5,
                operator: Token::Eq,
                right_value: 5,
            },
            Test {
                input: "5 != 5",
                left_value: 5,
                operator: Token::NotEq,
                right_value: 5,
            },
        ];

        for t in tests {
            let l = Lexer::new(t.input.into());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);

            let exp = match program.statements.first().unwrap() {
                Statement::Expression(stmt) => &stmt.expression,
                stmt => panic!("{:?} isn't an expression", stmt),
            };
            match exp {
                Expression::Infix(infix) => {
                    assert_eq!(
                        t.operator, infix.operator,
                        "expected {} operator but got {}",
                        t.operator, infix.operator
                    );
                    test_integer_literal(&infix.right_value, t.right_value);
                    test_integer_literal(&infix.left_value, t.left_value);
                }
                _ => panic!("expected infix operator but got {:?}", exp),
            }
        }
    }

    #[test]
    fn infix_boolean_literal_expression() {
        struct Test<'a> {
            input: &'a str,
            left_value: bool,
            operator: Token,
            right_value: bool,
        }

        let tests = vec![
            Test {
                input: "true == true",
                left_value: true,
                operator: Token::Eq,
                right_value: true,
            },
            Test {
                input: "true != false",
                left_value: true,
                operator: Token::NotEq,
                right_value: false,
            },
            Test {
                input: "false == false",
                left_value: false,
                operator: Token::Eq,
                right_value: false,
            },
        ];

        for test in tests {
            let l = Lexer::new(test.input.into());
            let mut p = Parser::new(l);

            let prog = p.parse_program();
            check_parser_errors(&p);

            let exp = match prog.statements.first().unwrap() {
                Statement::Expression(stmt) => &stmt.expression,
                stmt => panic!("{:?} isn't an expression", stmt),
            };
            match exp {
                Expression::Infix(infix) => {
                    assert_eq!(
                        test.operator, infix.operator,
                        "expected {} operator but got {}",
                        test.operator, infix.operator
                    );
                    test_boolean_literal(&infix.right_value, test.right_value);
                    test_boolean_literal(&infix.left_value, test.left_value);
                }
                _ => panic!("expected infix operator but got {:?}", exp),
            }
        }
    }

    #[test]
    fn operator_precedence_parsing() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let tests = vec![
            Test {
                input: "-a * b",
                expected: "((-a) * b)",
            },
            Test {
                input: "!-a",
                expected: "(!(-a))",
            },
            Test {
                input: "a + b + c",
                expected: "((a + b) + c)",
            },
            Test {
                input: "a + b - c",
                expected: "((a + b) - c)",
            },
            Test {
                input: "a * b * c",
                expected: "((a * b) * c)",
            },
            Test {
                input: "a * b / c",
                expected: "((a * b) / c)",
            },
            Test {
                input: "a + b / c",
                expected: "(a + (b / c))",
            },
            Test {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
            },
            Test {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4)((-5) * 5)",
            },
            Test {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4))",
            },
            Test {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4))",
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            Test {
                input: "true",
                expected: "true",
            },
            Test {
                input: "false",
                expected: "false",
            },
            Test {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false)",
            },
            Test {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true)",
            },
            Test {
                input: "1 + (2 + 3) + 4",

                expected: "((1 + (2 + 3)) + 4)",
            },
            Test {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2)",
            },
            Test {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5))",
            },
            Test {
                input: "-(5 + 5)",
                expected: "(-(5 + 5))",
            },
            Test {
                input: "!(true == true)",
                expected: "(!(true == true))",
            },
        ];

        for test in tests {
            let l = Lexer::new(test.input.into());
            let mut p = Parser::new(l);
            let prog = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(
                test.expected,
                prog.to_string(),
                "expected {} but got {}",
                test.expected,
                prog
            )
        }
    }

    #[test]
    fn bool_expression() {
        let input = "true;";
        let l = Lexer::new(input.into());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        let exp = match program.statements.first().unwrap() {
            Statement::Expression(stmt) => &stmt.expression,
            stmt => panic!("{:?} isn't an expression", stmt),
        };

        match exp {
            Expression::Boolean(b) => {
                assert_eq!(b, &true, "expected {} but got {}", true, b)
            }
            _ => panic!("expected boolean but got {:?}", exp),
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if !errors.is_empty() {
            panic!("got parser errors: {:?}", errors)
        }
    }

    fn test_integer_literal(int_lit: &Expression, value: i64) {
        match int_lit {
            Expression::Integer(int) => {
                assert_eq!(value, *int, "expected {} but got {}", value, int)
            }
            _ => panic!("expected integer literal {} but got {:?}", value, int_lit),
        }
    }

    fn test_boolean_literal(bool_lit: &Expression, value: bool) {
        match bool_lit {
            Expression::Boolean(b) => {
                assert_eq!(&value, b, "expected {} but got {}", value, b)
            }
            _ => panic!("expected boolean literal {} but got {:?}", value, bool_lit),
        }
    }

    fn test_indentifier(exp: &Expression, value: &str) {
        match exp {
            Expression::Identifier(ident) => {
                assert_eq!(value, ident, "expected {} got {}", value, ident)
            }
            _ => panic!("expected identifier but got {}", exp),
        }
    }
}
