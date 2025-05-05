use crate::token::Token;
use anyhow::Result;

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lex = Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lex.read_char();

        lex
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0
        } else {
            self.ch = self.input[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let tok = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b';' => Token::Semicolon,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b',' => Token::Comma,
            b'+' => Token::Plus,
            b'{' => Token::Lbrace,
            b'}' => Token::Rbrace,
            b'[' => Token::Lbracket,
            b']' => Token::Rbracket,
            b'-' => Token::Minus,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            b'/' => Token::Slash,
            b'*' => Token::Asterisk,
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return Ok(match ident.as_str() {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    "if" => Token::If,
                    "return" => Token::Return,
                    "true" => Token::True,
                    "false" => Token::False,
                    "else" => Token::Else,
                    _ => Token::Ident(ident),
                });
            }
            b'0'..=b'9' => return Ok(Token::Int(self.read_int())),
            b'"' => {
                let string = self.read_string();
                Token::String(string)
            }
            0 => Token::Eof,
            b':' => Token::Colon,
            _ => Token::Illegal,
        };
        self.read_char();
        Ok(tok)
    }

    fn read_ident(&mut self) -> String {
        let position = self.position;

        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char()
        }
        return String::from_utf8_lossy(&self.input[position..self.position]).to_string();
    }

    fn read_int(&mut self) -> i64 {
        let position = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char()
        }
        return String::from_utf8_lossy(&self.input[position..self.position])
            .parse::<i64>()
            .unwrap();
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }
        String::from_utf8_lossy(&self.input[position..self.position])
            .parse()
            .unwrap()
    }

    fn peek_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input[self.read_position]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}
