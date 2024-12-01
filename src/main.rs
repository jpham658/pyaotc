use compiler::Compiler;
use inkwell::context::Context;
use type_inference::{infer_types, Type, TypeEnv, TypeInferrer};
use rustpython_parser::{ast::{self, Stmt}, Parse};
mod astutils;
mod compiler;
mod type_rules;
mod type_inference;

fn types_pp(types: Vec<(Stmt, Type)>) {
    let table_header = "Statement number     | Type";
    println!("{}", table_header);
    println!("---------------------------------------------------------------------------");
    for (i, (_, typ)) in types.into_iter().enumerate() {
        let type_str = format!("{:?}", typ);
        let line_no = format!("{}", i + 1);
        let spaces = " ".repeat(20);
        println!("{}{}| {}", line_no, spaces, type_str);
    }
    println!()
}

fn main() {
    let python_source = r#"
def foo(x):
    return x + 1
y = foo(3)
"#;
    let context = Context::create();
    let compiler = Compiler::new(&context);
    let mut type_env = TypeEnv::new();
    let mut type_inferrer = TypeInferrer::new();
    let mut types: Vec<(Stmt,Type)> = Vec::new();

    match ast::Suite::parse(&python_source, "<embedded>") {
        Ok(ast) => {
            astutils::print_ast(&ast);
            for stmt in ast {
                match infer_types(&mut type_inferrer, &mut type_env, &stmt) {
                    Ok(typ) => {
                        types.push((stmt, typ));
                    },
                    Err(e) => {
                        eprintln!("{:?}", e);
                    }
                }
            }
            types_pp(types);
            // compiler.compile(ast);
        }
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
