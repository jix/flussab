use flussab::DeferredReader;

use flussab_cnf::{sat_solver_log, ParseError};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), ParseError> {
    let stdin = std::io::stdin();

    let ignore_unknown_lines = std::env::args().skip(1).any(|arg| &arg == "-u");

    let log = sat_solver_log::parse_log::<i32>(
        &mut DeferredReader::from_read(stdin.lock()).into(),
        sat_solver_log::Config::default().ignore_unknown_lines(ignore_unknown_lines),
    )?;

    println!("{log:?}");
    Ok(())
}
