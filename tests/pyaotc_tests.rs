use std::{fs, process::Command};

use lang_tester::LangTester;

fn main() {
    LangTester::new()
        .test_dir("examples")
        .test_path_filter(|p| p.extension().and_then(|x| x.to_str()) == Some("py"))
        .comment_prefix("#")
        .test_extract(|p| {
            fs::read_to_string(p)
                .unwrap()
                .lines()
                .skip_while(|l| !l.starts_with("#"))
                .take_while(|l| l.starts_with("#"))
                .map(|l| &l[1..])
                .collect::<Vec<_>>()
                .join("\n")
        })
        .test_cmds(move |p| {
            let test_file_path = p.to_str().expect("Path is not valid UTF-8");
            let error_msg = format!("Invalid file name {}", test_file_path);
            let binary_name = p.file_stem().and_then(|s| s.to_str()).expect(&error_msg);

            let mut compiler = Command::new("cargo");
            compiler.args(&["run", "--", test_file_path]);

            let binary_cmd = format!("./{}", binary_name);
            let runtime = Command::new(binary_cmd);

            vec![("Compiler", compiler), ("Run-time", runtime)]
        })
        .run();
}
