use std::{fs, path::PathBuf, process::Command};

use lang_tester::LangTester;

fn main() {
    let tempdir = "outputs";
    println!("executing tests");
    LangTester::new()
        .test_dir("examples")
        .test_path_filter(|p| p.extension().and_then(|x| x.to_str()) == Some("py"))
        .comment_prefix("#")
        .test_extract(|p| {
            fs::read_to_string(p)
                .unwrap()
                .lines()
                // Skip non-commented lines at the start of the file.
                .skip_while(|l| !l.starts_with("#"))
                // Extract consecutive commented lines.
                .take_while(|l| l.starts_with("#"))
                .map(|l| &l[1..])
                .collect::<Vec<_>>()
                .join("\n")
        })
        .test_cmds(move |p| {
            let test_file_path = p.to_str().expect("Path is not valid UTF-8");

            println!("About to compile test file: {}", test_file_path);
            
            let mut compiler = Command::new("cargo");
            compiler.args(&["run", "--", test_file_path]);
            println!("Running compiler command: cargo run -- {}", test_file_path);
            
            // TODO: Gotta replace this with the name of the file...
            let runtime = Command::new("./output");
            println!("Running runtime command: ./output {}", test_file_path);

            vec![("Compiler", compiler), ("Run-time", runtime)]
        })
        .run();
}
