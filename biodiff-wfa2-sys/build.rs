use std::path::PathBuf;

#[cfg(feature = "bundle-wfa2")]
fn link_wfa() {
    // we have our own custom CMakeLists.txt which we use to replace
    // the original
    println!("cargo:rerun-if-changed=CMakeLists.txt");
    let mut dst = cmake::Config::new(".").build();
    dst.push("lib");

    // search for the static library in the build directory

    println!("cargo:rustc-link-search=native={}", dst.display());
    // Link the `wfa-lib` library.
    println!("cargo:rustc-link-lib=static=wfa2");
}

#[cfg(not(feature = "bundle-wfa2"))]
fn link_wfa() {
    // Link the `wfa-lib` library.
    println!("cargo:rustc-link-lib=wfa2");
}

fn main() {
    if !PathBuf::from("WFA2-lib/CMakeLists.txt").exists() {
        eprintln!("The WFA2 submodule is not present. Please run `git submodule update --init` to fetch it.");
        std::process::exit(1);
    }
    link_wfa();
}
