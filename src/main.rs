use std::io;

use rust_interp::repl;

fn main() -> io::Result<()> {
    println!("Moneky REPL");
    let input = io::stdin();
    let output = io::stdout();
    let result = repl::start(input.lock(), output.lock());
    result
}
