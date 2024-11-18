use compiler::Compiler;
use inkwell::context::Context;
use rustpython_parser::{ast, Parse};
mod astutils;
mod compiler;
mod type_rules;

fn main() {
    //     let python_source = r#"
    // x = 3
    // x + 4.0
    // "#;
//     let python_source = r#"
// x = 3
// x = 2
// "#;
    let python_source = r#"
x = 3 / 1
"#;
    let context = Context::create();
    let compiler = Compiler::new(&context);

    match ast::Suite::parse(&python_source, "<embedded>") {
        Ok(ast) => {
            // astutils::print_ast(&ast);
            compiler.compile(ast);
        },
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
