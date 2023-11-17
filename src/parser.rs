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
    Call,
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
            Token::Lparen => Precedence::Call,
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
    ExpectedLparen(Token),
    ExpectedRparen(Token),
    ExpectedLbrace(Token),
    ExpectedRbrace(Token),
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

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let(LetStatement { name, value }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
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
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_function_literal),
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
            Token::Lparen => Some(Parser::parse_call_expression),
            _ => None,
        }
    }

    fn parse_identifier_into_identifier_expression(
        &mut self,
    ) -> Result<IdentifierExpression, ParserError> {
        if let Token::Ident(ref name) = self.curr_token {
            return Ok(IdentifierExpression {
                name: name.to_string(),
            });
        }

        Err(ParserError::IdentParse)
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

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call(Box::new(CallExpression {
            function,
            arguments,
        })))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut args: Vec<Expression> = Vec::new();

        if self.peek_token_is(&Token::Rparen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(Token::Rparen, ParserError::ExpectedRparen)?;

        Ok(args)
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        self.expect_peek(Token::Rparen, ParserError::ExpectedRparen)?;

        exp
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(Token::Lparen, ParserError::ExpectedLparen)?;

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::Rparen, ParserError::ExpectedRparen)?;
        self.expect_peek(Token::Lbrace, ParserError::ExpectedLbrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(&Token::Else) {
            self.next_token();
            self.expect_peek(Token::Lbrace, ParserError::ExpectedLbrace)?;

            let alt_block = self.parse_block_statement()?;
            Some(alt_block)
        } else {
            None
        };

        Ok(Expression::If(Box::new(IfExpression {
            condition,
            consequence,
            alternative,
        })))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(Token::Lparen, ParserError::ExpectedRparen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::Lbrace, ParserError::ExpectedRbrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::Function(Box::new(FunctionLiteral {
            parameters,
            body,
        })))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentifierExpression>, ParserError> {
        let mut identifiers: Vec<IdentifierExpression> = Vec::new();

        if self.peek_token_is(&Token::Rparen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        identifiers.push(self.parse_identifier_into_identifier_expression()?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(self.parse_identifier_into_identifier_expression()?);
        }

        self.expect_peek(Token::Rparen, ParserError::ExpectedRparen)?;

        Ok(identifiers)
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
        let mut statements = Vec::new();
        self.next_token();

        while !self.curr_token_is(&Token::Rbrace) && !self.curr_token_is(&Token::Eof) {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt)
            }
            self.next_token();
        }

        Ok(BlockStatement {
            statments: statements,
        })
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

    pub fn errors(&self) -> &Vec<ParserError> {
        &self.errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statement() {
        let prog = setup("let x = 5", 1);
        let exp = let_statemnent_parse_and_verify(&prog, "x");

        match exp {
            Expression::Integer(i) => assert_eq!(i, &5),
            _ => panic!("expected int expression"),
        }
    }

    #[test]
    fn let_statement_bool() {
        let prog = setup("let y = true", 1);
        let exp = let_statemnent_parse_and_verify(&prog, "y");
        match exp {
            Expression::Boolean(b) => assert_eq!(b, &true),
            _ => panic!("expected bool expression"),
        }
    }

    #[test]
    fn let_statement_ident() {
        let prog = setup("let foobar = y", 1);
        let exp = let_statemnent_parse_and_verify(&prog, "foobar");

        match exp {
            Expression::Identifier(id) => assert_eq!(id, "y"),
            _ => panic!("expected idetifier expression"),
        }
    }

    #[test]
    fn return_statement() {
        let program = setup(
            "return 5;
        return 10;
        return 993322;",
            3,
        );

        for stmt in program.statements {
            match stmt {
                Statement::Return(_) => {}
                _ => panic!("expected return statment but got {:?}", stmt),
            }
        }
    }

    #[test]
    fn return_statement_bool() {
        let prog = setup("return true", 1);
        let exp = return_statment_parse_and_verify(&prog);
        match exp {
            Expression::Boolean(b) => assert_eq!(b, &true),
            _ => panic!("not a boolean"),
        }
    }

    #[test]
    fn return_statement_ident() {
        let prog = setup("return foobar", 1);
        let exp = return_statment_parse_and_verify(&prog);

        match exp {
            Expression::Identifier(id) => assert_eq!(id, "foobar"),
            _ => panic!("not a identifier"),
        }
    }

    #[test]
    fn identifiers_expression() {
        let prog = setup("foobar", 1);
        let exp = unwrap_expression(&prog);

        test_indentifier(exp, "foobar");
    }

    #[test]
    fn integer_literals() {
        let prog = setup("5", 1);
        let exp = unwrap_expression(&prog);

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
            let prog = setup(t.input, 1);
            let exp = unwrap_expression(&prog);

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
            let prog = setup(t.input, 1);
            let exp = unwrap_expression(&prog);
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
            let prog = setup(test.input, 1);
            let exp = unwrap_expression(&prog);

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
            Test {
                input: "a + add(b * c) + d",
                expected: "((a + add((b * c))) + d)",
            },
            Test {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            },
            Test {
                input: "add(a + b + c * d / f + g)",
                expected: "add((((a + b) + ((c * d) / f)) + g))",
            },
        ];

        for test in tests {
            let prog = setup(test.input, 0);

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
        let program = setup("true", 1);
        let exp = unwrap_expression(&program);

        match exp {
            Expression::Boolean(b) => {
                assert_eq!(b, &true, "expected {} but got {}", true, b)
            }
            _ => panic!("expected boolean but got {:?}", exp),
        }
    }

    #[test]
    fn if_expression() {
        let prog = setup("if (x < y) { x }", 1);
        let exp = unwrap_expression(&prog);

        match exp {
            Expression::If(ifexp) => {
                test_if(&ifexp.condition, "x", Token::Lt, "y");

                assert_eq!(
                    ifexp.consequence.statments.len(),
                    1,
                    "expected only 1 statement"
                );
                match ifexp.consequence.statments.first().unwrap() {
                    Statement::Expression(stmt) => test_indentifier(&stmt.expression, "x"),
                    stmt => panic!("expected expression statement but got {:?}", stmt),
                }
                if let Some(stmt) = &ifexp.alternative {
                    panic!("expected no alternative statement but got {:?}", stmt)
                }
            }
            _ => panic!("expected if expression but got {:?}", exp),
        }
    }

    #[test]
    fn if_else_expression() {
        let prog = setup("if (x < y) { x } else { y }", 1);
        let exp = unwrap_expression(&prog);

        match exp {
            Expression::If(ifexp) => {
                test_if(&ifexp.condition, "x", Token::Lt, "y");

                assert_eq!(
                    ifexp.consequence.statments.len(),
                    1,
                    "expected only 1 statement"
                );
                match ifexp.consequence.statments.first().unwrap() {
                    Statement::Expression(stmt) => test_indentifier(&stmt.expression, "x"),
                    stmt => panic!("expected expression statement but got {:?}", stmt),
                }
                if let Some(stmt) = &ifexp.alternative {
                    assert_eq!(stmt.statments.len(), 1, "expected only 1 statement");
                    match stmt.statments.first().unwrap() {
                        Statement::Expression(stmt) => test_indentifier(&stmt.expression, "y"),
                        stmt => panic!("expected expression statement but got {:?}", stmt),
                    }
                } else {
                    panic!("expected alternative block")
                }
            }
            _ => panic!("expected if expression but got {:?}", exp),
        }
    }

    #[test]
    fn function_literal_parsing() {
        let prog = setup("fn(x, y) { x + y; }", 1);
        let exp = unwrap_expression(&prog);

        match exp {
            Expression::Function(func) => {
                assert_eq!(
                    2,
                    func.parameters.len(),
                    "expected 2 parameters but got {:?}",
                    func.parameters.len()
                );
                assert_eq!(func.parameters.first().unwrap().name, "x");
                assert_eq!(func.parameters.last().unwrap().name, "y");
                assert_eq!(
                    1,
                    func.body.statments.len(),
                    "expected 1 body statement but got {:?}",
                    func.body.statments.len()
                );

                match func.body.statments.first().unwrap() {
                    Statement::Expression(stmt) => match &stmt.expression {
                        Expression::Infix(infix) => {
                            assert_eq!(
                                infix.operator,
                                Token::Plus,
                                "expected + but got {}",
                                infix.operator
                            );
                            test_indentifier(&infix.left_value, "x");
                            test_indentifier(&infix.right_value, "y");
                        }
                        _ => panic!("expected infix but got {:?}", stmt.expression),
                    },
                    stmt => panic!("expected expression but got {:?}", stmt),
                }
            }
            _ => panic!("{} is not a function literal", exp),
        }
    }

    #[test]
    fn function_parameters_parsing() {
        struct Test<'a> {
            input: &'a str,
            expected: Vec<&'a str>,
        }

        let tests = vec![
            Test {
                input: "fn() {};",
                expected: vec![],
            },
            Test {
                input: "fn(x) {};",
                expected: vec!["x"],
            },
            Test {
                input: "fn(x, y, z) {};",
                expected: vec!["x", "y", "z"],
            },
        ];

        for test in tests {
            let prog = setup(test.input, 1);
            let exp = unwrap_expression(&prog);

            match exp {
                Expression::Function(func) => {
                    assert_eq!(func.parameters.len(), test.expected.len());

                    let mut params = test.expected.into_iter();
                    for param in &func.parameters {
                        let expected_param = params.next().unwrap();
                        assert_eq!(expected_param, param.name.as_str());
                    }
                }
                _ => panic!("{:?} not a function literal", exp),
            }
        }
    }

    #[test]
    fn call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);".to_string();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        let exp = match prog.statements.first().unwrap() {
            Statement::Expression(stmt) => &stmt.expression,
            stmt => panic!("{:?} is not an expression", stmt),
        };

        match exp {
            Expression::Call(call) => {
                test_indentifier(&call.function, "add");
                assert_eq!(3, call.arguments.len());
                let mut args = call.arguments.iter();
                test_integer_literal(args.next().unwrap(), 1);
                test_infix(args.next().unwrap(), 2, Token::Asterisk, 3);
                test_infix(args.next().unwrap(), 4, Token::Plus, 5);
            }
            _ => panic!("{:?} is not a call expression", exp),
        }
    }

    #[test]
    fn call_expression_arguments() {
        struct Test<'a> {
            input: &'a str,
            expected_ident: &'a str,
            expected_args: Vec<&'a str>,
        }

        let tests = vec![
            Test {
                input: "add();",
                expected_ident: "add",
                expected_args: vec![],
            },
            Test {
                input: "add(1);",
                expected_args: vec!["1"],
                expected_ident: "add",
            },
            Test {
                input: "add(1, 2 * 3, 4 + 5);",
                expected_ident: "add",
                expected_args: vec!["1", "(2 * 3)", "(4 + 5)"],
            },
        ];

        for test in tests {
            let prog = setup(test.input, 1);
            let exp = unwrap_expression(&prog);

            match exp {
                Expression::Call(call) => {
                    test_indentifier(&call.function, test.expected_ident);
                    assert_eq!(call.arguments.len(), test.expected_args.len());
                    let mut args = call.arguments.iter();
                    for a in test.expected_args {
                        assert_eq!(a.to_string(), args.next().unwrap().to_string());
                    }
                }
                _ => panic!("{:?} is not a call expression", exp),
            }
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if !errors.is_empty() {
            panic!("got parser errors: {:?}", errors)
        }
    }

    fn test_infix(exp: &Expression, left: i64, op: Token, right: i64) {
        match exp {
            Expression::Infix(infix) => {
                assert_eq!(
                    op, infix.operator,
                    "expected {} operator but got {}",
                    op, infix.operator
                );
                test_integer_literal(&infix.left_value, left);
                test_integer_literal(&infix.right_value, right);
            }
            exp => panic!("expected infix expression but got {:?}", exp),
        }
    }

    fn test_if(exp: &Expression, left: &str, op: Token, right: &str) {
        match exp {
            Expression::Infix(infix) => {
                test_indentifier(&infix.left_value, left);
                test_indentifier(&infix.right_value, right);
                if op != infix.operator {
                    panic!("expected {} but got {}", op, infix.operator)
                }
            }
            _ => panic!("expected infix expression but got {:?}", exp),
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

    fn let_statemnent_parse_and_verify<'a>(
        prog: &'a Program,
        expected_ident: &str,
    ) -> &'a Expression {
        let stmt = prog.statements.first().unwrap();
        match stmt {
            Statement::Let(stmt) => {
                assert_eq!(stmt.name.as_str(), expected_ident);
                &stmt.value
            }
            stmt => panic!("expected let statment but got {:?}", stmt),
        }
    }

    fn return_statment_parse_and_verify(prog: &Program) -> &Expression {
        let stmt = prog.statements.first().unwrap();
        match stmt {
            Statement::Return(stmt) => &stmt.value,
            stmt => panic!("expected return statement but got {:?}", stmt),
        }
    }

    fn setup(input: &str, stmt_count: usize) -> Program {
        let l = Lexer::new(input.into());
        let mut p = Parser::new(l);
        let prog = p.parse_program();
        check_parser_errors(&p);

        if stmt_count != 0 && prog.statements.len() != stmt_count {
            panic!(
                "expected {} statement(s) for '{}' but got {:?}",
                stmt_count, input, prog.statements
            )
        }

        prog
    }

    fn unwrap_expression(prog: &Program) -> &Expression {
        match prog.statements.first().unwrap() {
            Statement::Expression(stmt) => &stmt.expression,
            stmt => panic!("{:?} isn't an expression", stmt),
        }
    }
}
