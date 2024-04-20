use std::path::PathBuf;

#[cfg(feature = "bundle-wfa2")]
fn link_wfa() {
    // we have our own custom CMakeLists.txt which we use to replace
    // the original
    println!("cargo:rerun-if-changed=CMakeLists.txt");
    let mut dst = cmake::Config::new(".").build_target("wfa2_static").build();
    dst.push("build");

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

fn generate_wfa_bindings() {
    let bindings = bindgen::Builder::default()
        .header("WFA2-lib/utils/commons.h")
        // Generate bindings for this header file.
        .header("WFA2-lib/wavefront/wavefront_align.h")
        // Add this directory to the include path to find included header files.
        .clang_arg("-IWFA2-lib")
        // Generate bindings for all functions starting with `wavefront_`.
        .allowlist_function("wavefront_.*")
        // Generate bindings for all variables starting with `wavefront_`.
        .allowlist_var("wavefront_.*")
        // Invalidate the built crate whenever any of the included header files
        // changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings_wfa.rs file.
    let out_path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings_wfa2.rs"))
        .expect("Couldn't write bindings!");
}

fn main() {
    if !PathBuf::from("WFA2-lib/CMakeLists.txt").exists() {
        eprintln!("The WFA2 submodule is not present. Please run `git submodule update --init` to fetch it.");
        std::process::exit(1);
    }
    link_wfa();
    generate_wfa_bindings();
}
