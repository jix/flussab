use flussab::DeferredWriter;

use flussab_aiger::{ascii, binary, ParseError};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), ParseError> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    let aig_reader = binary::Parser::<u32>::from_read(stdin.lock(), binary::Config::default())?;

    let aig = aig_reader.parse()?;

    let mut aag_writer = DeferredWriter::from_write(stdout.lock());
    let aag_writer = ascii::Writer::<u32>::new(&mut aag_writer);

    aag_writer.write_ordered_aig(&aig);
    Ok(())
}
