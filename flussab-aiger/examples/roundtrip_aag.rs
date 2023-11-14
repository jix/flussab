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
    let mut aag_writer = DeferredWriter::from_write(stdout.lock());
    let aag_writer = ascii::Writer::<u32>::new(&mut aag_writer);

    aag_writer.write_header(aag_reader.header());

    let mut aag_reader = aag_reader.inputs()?;
    while let Some(input) = aag_reader.next_input()? {
        aag_writer.write_lit(input);
    }

    let mut aag_reader = aag_reader.latches()?;
    while let Some(latch) = aag_reader.next_latch()? {
        aag_writer.write_latch(latch);
    }

    let mut aag_reader = aag_reader.outputs()?;
    while let Some(output) = aag_reader.next_output()? {
        aag_writer.write_lit(output);
    }

    let mut aag_reader = aag_reader.bad_state_properties()?;
    while let Some(bad_state_property) = aag_reader.next_bad_state_property()? {
        aag_writer.write_lit(bad_state_property);
    }

    let mut aag_reader = aag_reader.invariant_constraints()?;
    while let Some(invariant_constraint) = aag_reader.next_invariant_constraint()? {
        aag_writer.write_lit(invariant_constraint);
    }

    let mut aag_reader = aag_reader.justice_properties()?;
    while let Some(justice_property_size) = aag_reader.next_justice_property_size()? {
        aag_writer.write_count(justice_property_size);
    }

    let mut aag_reader = aag_reader.justice_property_local_fairness_constraints()?;
    while let Some(justice_property_local_fairness_constraint) =
        aag_reader.next_justice_property_local_fairness_constraint()?
    {
        aag_writer.write_lit(justice_property_local_fairness_constraint);
    }

    let mut aag_reader = aag_reader.fairness_constraints()?;
    while let Some(fairness_constraint) = aag_reader.next_fairness_constraint()? {
        aag_writer.write_lit(fairness_constraint);
    }

    let mut aag_reader = aag_reader.and_gates()?;
    while let Some(and_gate) = aag_reader.next_and_gate()? {
        aag_writer.write_and_gate(and_gate);
    }

    let mut aag_reader = aag_reader.symbols()?;
    while let Some(symbol) = aag_reader.next_symbol()? {
        aag_writer.write_symbol(&symbol);
    }

    if let Some(comment) = aag_reader.comment()? {
        aag_writer.write_comment(comment);
    }

    Ok(())
}
