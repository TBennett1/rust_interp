use rust_interp::lexer::Lexer;
use rust_interp::token::Token;
use std::io;

fn main() -> io::Result<()> {
    io::stdin().lines().for_each(|line| {
        if let Ok(line) = line {
            let mut tokenizer = Lexer::new(line);

            while let Ok(token) = tokenizer.next_token() {
                println!("{} ", token);
                if let Token::Eof = token {
                    break;
                }
            }
        }
    });
    Ok(())
}
