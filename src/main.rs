use compiler::Compiler;
use inkwell::context::Context;
use type_inference::TypeEnv;
use rustpython_parser::{ast, Parse};
mod astutils;
mod compiler;
mod type_rules;
mod type_inference;

fn main() {
    let python_source = r#"
def add(x: 'int'):
    return 1 + 1
add(1)
"#;
    let context = Context::create();
    let compiler = Compiler::new(&context);

    match ast::Suite::parse(&python_source, "<embedded>") {
        Ok(ast) => {
            astutils::print_ast(&ast);
            compiler.compile(ast);
        }
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
