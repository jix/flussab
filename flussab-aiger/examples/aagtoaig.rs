use std::io::Write;

use flussab::DeferredWriter;

use flussab_aiger::{
    aig::{Renumber, RenumberConfig},
    ascii, binary, Error,
};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), Error<u32>> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    let aag_reader = ascii::Parser::<u32>::from_read(stdin.lock(), ascii::Config::default())?;

    let aag = aag_reader.parse()?;

    let (aig, _) = Renumber::renumber_aig(
        RenumberConfig::default()
            .trim(true)
            .structural_hash(true)
            .const_fold(true),
        &aag,
    )?;

    let aig_writer = DeferredWriter::from_write(stdout.lock());
    let mut aag_writer = binary::Writer::<u32>::new(aig_writer);

    aag_writer.write_ordered_aig(&aig);

    aag_writer.flush()?;
    Ok(())
}
