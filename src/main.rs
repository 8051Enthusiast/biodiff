mod align;
mod backend;
mod control;
mod cursor;
mod datastruct;
mod dialog;
mod doublehex;
mod file;
mod search;
mod selection;
mod style;
mod util;
mod view;
use std::env;
use std::ffi::OsString;
use std::process::exit;

use file::FileState;

fn print_usage(name: &OsString) -> ! {
    eprintln!("usage: {} [file1] [file2]", name.to_string_lossy());
    exit(1)
}

fn main() {
    let args: Vec<_> = env::args_os().collect();
    // we expect exactly two arguments, being the files
    // might extend this in the future, but for now this is enough
    let (xfile, yfile) = match &args[1..] {
        [s] => {
            if matches!(s.to_str(), Some("-v" | "--version")) {
                println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
                exit(0);
            } else {
                print_usage(&args[0])
            }
        }
        [a, b] => (a, b),
        _otherwise => print_usage(&args[0]),
    };
    let x = FileState::from_file(xfile).unwrap_or_else(|e| {
        eprintln!("Could not read {}: {}", xfile.to_string_lossy(), e);
        exit(1);
    });
    let y = FileState::from_file(yfile).unwrap_or_else(|e| {
        eprintln!("Could not read {}: {}", yfile.to_string_lossy(), e);
        exit(1);
    });
    // main control loop
    control::run(x, y)
}
