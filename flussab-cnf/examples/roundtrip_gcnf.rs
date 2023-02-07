use std::io::Write;

use flussab::DeferredWriter;

use flussab_cnf::{gcnf, ParseError};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), ParseError> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    let mut gcnf_reader = gcnf::Parser::<i32>::from_read(stdin.lock(), gcnf::Config::default())?;
    let mut gcnf_writer = DeferredWriter::from_write(stdout.lock());

    if let Some(header) = gcnf_reader.header() {
        gcnf::write_header(&mut gcnf_writer, header);
    }

    while let Some((group, lits)) = gcnf_reader.next_clause()? {
        gcnf::write_clause(&mut gcnf_writer, group, lits);
    }

    gcnf_writer.flush()?;
    Ok(())
}
