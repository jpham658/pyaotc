use compiler::Compiler;
use inkwell::context::Context;
use rustpython_parser::{ast, Parse};
mod astutils;
mod compiler;
mod type_rules;

fn main() {
    //     let python_source = r#"
    // x = 3
    // x = 2
    // "#;
    let python_source = r#"
len(x)
"#;
    let context = Context::create();
    let compiler = Compiler::new(&context);

    match ast::Suite::parse(&python_source, "<embedded>") {
        Ok(ast) => {
            astutils::print_ast(&ast);
            println!("{:?}", type_rules::build_variable_rules(&ast));
            // compiler.compile(ast);
        }
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
