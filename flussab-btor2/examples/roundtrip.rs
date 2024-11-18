use std::io::Write;

use flussab::DeferredWriter;

use flussab_btor2::{ParseError, Parser};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), ParseError> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    let mut parser = Parser::from_read(stdin, Default::default())?;
    let mut target = DeferredWriter::from_write(stdout);

    while let Some(line) = parser.next_line()? {
        line.write_into(&mut target);
    }
    target.flush()?;

    Ok(())
}
