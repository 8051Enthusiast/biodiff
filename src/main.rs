mod align;
mod backend;
mod config;
mod control;
mod cursor;
mod datastruct;
mod dialog;
mod doublehex;
mod file;
mod preset;
mod print;
mod search;
mod selection;
mod style;
mod util;
mod view;
use std::ffi::OsString;
use std::path::PathBuf;
use std::process::exit;
use std::{env, ffi::OsStr};

use clap::Parser;
use config::get_settings;
use file::FileState;

fn load_file(file: &OsStr) -> FileState {
    FileState::from_file(&PathBuf::from(file)).unwrap_or_else(|e| {
        eprintln!("Could not read {}: {}", file.to_string_lossy(), e);
        exit(1);
    })
}

#[derive(Debug, Default, clap::Parser)]
#[command(version)]
struct Args {
    /// Print the diff to stdout instead of showing the UI
    #[clap(short, long)]
    print: bool,
    /// Number of columns to display
    #[clap(short, long)]
    columns: Option<u16>,
    /// Name of the global preset to use instead of the default
    #[clap(short, long)]
    global_preset: Option<String>,
    /// Name of the semiglobal preset to use instead of the default
    #[clap(short, long)]
    semiglobal_preset: Option<String>,
    /// Use color output for --print even if stdout is not a terminal/NO_COLOR is set
    #[clap(short = 'C', long)]
    color: bool,
    /// Use a specific config file instead of the default
    #[clap(long)]
    config: Option<PathBuf>,
    file1: OsString,
    file2: OsString,
}

fn main() {
    let args: Vec<_> = env::args_os().collect();
    let args = match &args[1..] {
        // for backwards compability
        [x, y] => Args {
            file1: x.to_owned(),
            file2: y.to_owned(),
            ..Args::default()
        },
        _ => Args::parse(),
    };
    let x = load_file(&args.file1);
    let y = load_file(&args.file2);
    let settings = get_settings(&args).unwrap_or_else(|e| {
        eprintln!("Error loading settings: {}", e);
        exit(1);
    });
    if args.print {
        print::print(settings, x.content, y.content, args.color)
    } else {
        // main control loop
        control::run(settings, x, y)
    }
}
