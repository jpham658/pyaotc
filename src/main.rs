#![deny(elided_lifetimes_in_paths)]

mod astutils;
mod call_collector;
mod codegen;
mod compiler;
mod compiler_utils;
mod rule_typing;
mod type_inference;

use astutils::print_ast;
use call_collector::FunctionCallCollector;
use compiler::Compiler;
use inkwell::context::Context;
use rustpython_parser::{
    ast::{self},
    Parse,
};
use std::{env, fs, path::Path};
use type_inference::{free_type_vars_in_type_env, infer_stmts, NodeTypeDB, TypeEnv, TypeInferrer};

fn main() {
    let args: Vec<String> = env::args().collect();
    // if args.len() <= 1 {
    //     panic!("Please enter a Python file to compile.");
    // }

    let python_source_path = if args.len() > 1 { &args[1] } else { "test.py" };
    let python_source = fs::read_to_string(python_source_path)
        .expect(format!("Could not read file {}", &python_source_path).as_str());
    if Path::new(python_source_path)
        .extension()
        .and_then(|x| x.to_str())
        != Some("py")
    {
        panic!("Given file is not a Python file.");
    }
    let file_name = {
        Path::new(python_source_path)
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("Invalid file path.")
    };
    let context = Context::create();

    // TODO: Refactor to take flag from command args
    // to decide if we generically compile or not...
    let mut compiler = Compiler::new(&context, false);
    let mut type_env = TypeEnv::new();
    let mut type_inferrer = TypeInferrer::new();
    let mut type_db = NodeTypeDB::new();

    match ast::Suite::parse(&python_source, &python_source_path) {
        Ok(ast) => {
            print_ast(&ast);
            // normal type inference
            infer_stmts(&mut type_inferrer, &mut type_env, &ast, &mut type_db);
            println!("type env: {:?}", type_env);

            // TODO: Implement inference with timeout
            // TODO: Add call collector support to type inferrer
            // if we still have unbound types, use call collector to instantiate functions with
            // most common argument types
            // while (!free_type_vars_in_type_env(&type_env).is_empty() || timeout_check()) {
            //     let mut call_collector = FunctionCallCollector::new(&type_db);
            //     call_collector.collect_calls(&ast);
            //     println!("{:?}", call_collector.most_common_arg_types());
            //     infer_stmts(&mut type_inferrer, &mut type_env, &ast, &mut type_db);
            // }

            compiler.compile(&ast, &type_db, file_name);
            // compiler.compile_generically(&ast, file_name);
        }
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
