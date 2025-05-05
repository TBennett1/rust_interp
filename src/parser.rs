use std::{collections::HashMap, mem};

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
    Index,
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
            Token::Lbracket => Precedence::Index,
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
    StrLitParse,
    ExpectedLparen(Token),
    ExpectedRparen(Token),
    ExpectedLbrace(Token),
    ExpectedRbrace(Token),
    ExpectedRbracket(Token),
    ExpectedColon(Token),
    ExpectedComma(Token),
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
            Token::String(_) => Some(Parser::parse_string_literal),
            Token::Lbracket => Some(Parser::parse_array_literal),
            Token::Lbrace => Some(Parser::parse_hash_literal),
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
            Token::Lbracket => Some(Parser::parse_index_expression),
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
        let arguments = self.parse_expression_list(&Token::Rparen)?;
        Ok(Expression::Call(Box::new(CallExpression {
            function,
            arguments,
        })))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::Rbracket, ParserError::ExpectedRbracket)?;

        Ok(Expression::Index(Box::new(IndexExpression { left, index })))
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

    fn parse_string_literal(&mut self) -> Result<Expression, ParserError> {
        if let Token::String(ref val) = self.curr_token {
            return Ok(Expression::String(val.clone()));
        }

        Err(ParserError::StrLitParse)
    }

    fn parse_array_literal(&mut self) -> Result<Expression, ParserError> {
        let elements = self.parse_expression_list(&Token::Rbracket)?;

        Ok(Expression::Array(Box::new(ArrayLiteral { elements })))
    }

    fn parse_expression_list(&mut self, end: &Token) -> Result<Vec<Expression>, ParserError> {
        let mut expressions = Vec::new();

        if self.peek_token_is(end) {
            self.next_token();
            return Ok(expressions);
        }

        self.next_token();
        expressions.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            expressions.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end.clone(), ParserError::ExpectedRbracket)?;

        Ok(expressions)
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, ParserError> {
        let mut hash = HashMap::new();

        while !self.peek_token_is(&Token::Rbrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;

            self.expect_peek(Token::Colon, ParserError::ExpectedColon)?;

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            hash.insert(key, value);

            if !self.peek_token_is(&Token::Rbrace)
                && self
                    .expect_peek(Token::Comma, ParserError::ExpectedComma)
                    .is_err()
            {
                return Err(ParserError::ExpectedComma(self.curr_token.clone()));
            }
        }

        self.expect_peek(Token::Rbrace, ParserError::ExpectedRbrace)?;

        Ok(Expression::Hash(HashLiteral { pairs: hash }))
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
