mod lib;
use lib::hexagex;
fn main() {
    if let [_, regex, file] = &std::env::args().collect::<Vec<_>>()[..] {
        let content = match std::fs::read(file) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Could not read file \"{}\": {}", file, e);
                std::process::exit(1);
            }
        };
        let regex = match hexagex(regex) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(2)
            }
        };
        for m in regex.find_iter(&content) {
            println!("{:#04x}: {}", m.start(), hex::encode(m.as_bytes()));
        }
    } else {
        println!("Usage: hexagex [regex] [file]");
        std::process::exit(3);
    }
}