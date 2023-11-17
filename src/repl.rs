use crate::evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io;

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
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

        let evaluated = evaluator::eval(program);
        writeln!(writer, "{}", evaluated)?
    }

    Ok(())
}
