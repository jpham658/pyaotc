use std::{fs, process::Command};

use lang_tester::LangTester;

fn main() {
    let tempdir = "outputs";
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
            let mut compiler = Command::new("cargo");
            compiler.args(&["run", "--", p.to_str().unwrap()]);
            let mut runtime = Command::new("lli");
            runtime.args(&["output.ll"]);
            vec![("Compiler", compiler), ("Run-time", runtime)]
        })
        .run();
}
