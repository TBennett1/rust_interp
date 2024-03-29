use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::{evaluator, object};
use std::cell::RefCell;
use std::io;
use std::rc::Rc;

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    let env = Rc::new(RefCell::new(object::Environment::new()));
    loop {
        writer.write_all(b"> ")?;
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;

        if line.trim() == "quit" {
            break;
        }

        let l = Lexer::new(line);
        let mut p = Parser::new(l);

        let program = p.parse_program();

        if !p.errors().is_empty() {
            writeln!(writer, "\t{:?}", p.errors())?;
            continue;
        }

        let evaluated = evaluator::eval(
            &crate::ast::Node::Program(Box::new(program)),
            Rc::clone(&env),
        )
        .unwrap();
        writeln!(writer, "{}", evaluated)?
    }

    Ok(())
}
