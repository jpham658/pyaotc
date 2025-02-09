#![deny(elided_lifetimes_in_paths)]

mod astutils;
mod codegen;
mod compiler;
mod compiler_utils;
mod type_inference;

use compiler::Compiler;
use inkwell::context::Context;
use rustpython_parser::{
    ast::{self, Stmt},
    Parse,
};
use std::collections::HashMap;
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
x = 2
def foo(x):
    while (x):
        print(x)
        x = x - 1
    else:
        print(x)
foo(2.0)
"#;
    let context = Context::create();
    let compiler = Compiler::new(&context);
    let mut type_env = TypeEnv::new();
    let mut type_inferrer = TypeInferrer::new();
    let mut types: HashMap<String, Type> = HashMap::new();

    match ast::Suite::parse(&python_source, "<embedded>") {
        Ok(ast) => {
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
            compiler.compile(&ast, &types);
            // compiler.compile_generically(&ast);
        }
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
