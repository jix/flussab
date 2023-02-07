use std::io::Write;

use flussab::DeferredWriter;

use flussab_cnf::{wcnf, ParseError};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), ParseError> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    let mut wcnf_reader = wcnf::Parser::<i32>::from_read(stdin.lock(), wcnf::Config::default())?;
    let mut wcnf_writer = DeferredWriter::from_write(stdout.lock());

    if let Some(header) = wcnf_reader.header() {
        wcnf::write_header(&mut wcnf_writer, header);
    }

    while let Some((weight, lits)) = wcnf_reader.next_clause()? {
        wcnf::write_clause(&mut wcnf_writer, weight, lits);
    }

    wcnf_writer.flush()?;
    Ok(())
}
