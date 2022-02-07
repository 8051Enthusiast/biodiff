mod align;
mod backend;
mod control;
mod datastruct;
mod dialog;
mod drawer;
mod file;
mod search;
mod view;
mod util;
use std::env;
use std::process::exit;

use file::FileState;

fn main() {
    let args: Vec<_> = env::args_os().collect();
    // we expect exactly two arguments, being the files
    // might extend this in the future, but for now this is enough
    let (xfile, yfile) = match &args[1..] {
        [a, b] => (a, b),
        _otherwise => {
            eprintln!("usage: {} [file1] [file2]", args[0].to_string_lossy());
            exit(1);
        }
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
