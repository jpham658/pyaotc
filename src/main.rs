mod astutils;
mod compiler;
mod type_inference;
mod compiler_utils;
mod codegen;

use std::collections::HashMap;
use compiler::Compiler;
use inkwell::context::Context;
use rustpython_parser::{
    ast::{self, Stmt},
    Parse,
};
use type_inference::{infer_types, Type, TypeEnv, TypeInferrer};

fn types_pp(name_to_type: &HashMap<String, Type>) {
    println!("Name                          Type");
    println!("-----------------------------------");
    for (name, typ) in name_to_type.into_iter() {
        println!("{}                          {:?}", name, typ);
    }
    println!()
}

fn main() {
    let python_source = r#"
def add(x):
    return x + 1

add(3 + 2 + 1)
add(1.0)
"#;
    let context = Context::create();
    let compiler = Compiler::new(&context);
    let mut type_env = TypeEnv::new();
    let mut type_inferrer = TypeInferrer::new();
    let mut types: HashMap<String, Type> = HashMap::new();

    match ast::Suite::parse(&python_source, "<embedded>") {
        Ok(ast) => {
            // right now we are at the top scope level...
            // TODO: figure out how to handle scope in TypeEnv and compiler
            astutils::print_ast(&ast);
            for stmt in &ast {
                match infer_types(&mut type_inferrer, &mut type_env, &stmt) {
                    Ok(typ) => match &stmt {
                        Stmt::FunctionDef(funcdef) => {
                            let func_name = funcdef.name.as_str().to_string();
                            types.insert(func_name, typ);
                        }
                        Stmt::Assign(assign) => {
                            let lhs_name = assign.targets[0]
                                .as_name_expr()
                                .unwrap()
                                .id
                                .as_str()
                                .to_string();
                            types.insert(lhs_name, typ);
                        }
                        _ => {}
                    },
                    Err(e) => {
                        eprintln!("{:?}", e);
                    }
                }
            }
            types_pp(&types);
            compiler.compile_with_types(&ast, &types);
        }
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
