use std::env;
use std::process::exit;
use std::process::Command;

fn main() {
    let status = Command::new("git")
        .args(&["difftool", "--no-prompt", "--extcmd=biodiff"])
        .args(env::args_os().skip(1))
        .status()
        .expect("Failed to run git");
    exit(status.code().expect("Terminated by signal"));
}
