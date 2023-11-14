use flussab::DeferredWriter;

use flussab_aiger::{ascii, ParseError};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), ParseError> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    let aag_reader = ascii::Parser::<u32>::from_read(stdin.lock(), ascii::Config::default())?;

    let aig = aag_reader.parse()?;

    let mut aag_writer = DeferredWriter::from_write(stdout.lock());
    let aag_writer = ascii::Writer::<u32>::new(&mut aag_writer);

    aag_writer.write_aig(&aig);
    Ok(())
}
