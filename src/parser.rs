use std::mem;

use anyhow::Result;

use crate::{
    ast::{self, Program},
    lexer::Lexer,
    token::Token,
};

struct Parser {
    lexer: Lexer,
    errors: Vec<ParserError>,
    curr_token: Token,
    peek_token: Token,
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedIdentifierToken(Token),
    ExpectedAssign(Token),
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

    pub fn parse_program(&mut self) -> ast::Program {
        let mut statements = vec![];

        while self.curr_token != Token::Eof {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.next_token()
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        match self.curr_token {
            Token::Let => return self.parse_let_statement(),
            _ => todo!(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement, ParserError> {
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

        while self.curr_token != Token::Semicolon {
            self.next_token();
        }

        return Ok(ast::Statement::Let(name));
    }

    fn expect_peek(
        &mut self,
        token: Token,
        expected: fn(Token) -> ParserError,
    ) -> Result<(), ParserError> {
        if self.peek_token != token {
            return Err(expected(self.peek_token.clone()));
        }
        self.next_token();
        Ok(())
    }

    fn errors(&self) -> &[ParserError] {
        &self.errors
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Statement};

    use super::*;

    #[test]
    fn test_let_statement() {
        let input = "
            let x 5;
            let y = 10;
            let 838383;
        ";
        let l = Lexer::new(input.into());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements,
            vec![
                Statement::Let("x".to_string()),
                Statement::Let("y".to_string()),
                Statement::Let("foobar".to_string())
            ]
        )
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if errors.len() > 0 {
            panic!("got parser errors: {:?}", errors)
        }
    }
}
