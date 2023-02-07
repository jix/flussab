use std::io::Write;

use flussab::DeferredWriter;

use flussab_cnf::{cnf, ParseError};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), ParseError> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    let mut cnf_reader = cnf::Parser::<i32>::from_read(stdin.lock(), cnf::Config::default())?;
    let mut cnf_writer = DeferredWriter::from_write(stdout.lock());

    if let Some(header) = cnf_reader.header() {
        cnf::write_header(&mut cnf_writer, header);
    }

    while let Some(lits) = cnf_reader.next_clause()? {
        cnf::write_clause(&mut cnf_writer, lits);
    }

    cnf_writer.flush()?;
    Ok(())
}
