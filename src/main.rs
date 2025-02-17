#![deny(elided_lifetimes_in_paths)]

mod astutils;
mod codegen;
mod compiler;
mod compiler_utils;
mod rule_typing;
mod type_inference;

use compiler::Compiler;
use inkwell::context::Context;
use rule_typing::{get_inferred_rule_types, RuleEnv};
use rustpython_parser::{
    ast::{self},
    Parse,
};
use std::{collections::HashMap, env, fs};
use type_inference::{infer_ast_types, NodeTypeDB, TypeEnv, TypeInferrer};


fn main() {
    let args: Vec<String> = env::args().collect();
    let python_source_path = if args.len() > 1 {&args[1]} else {"test.py"};
    let python_source = fs::read_to_string(python_source_path)
        .expect(format!("Could not read file {}", &python_source_path).as_str());

    let context = Context::create();
    let compiler = Compiler::new(&context);
    let mut type_env = TypeEnv::new();
    let mut type_inferrer = TypeInferrer::new();
    let mut type_db = NodeTypeDB::new();
    let mut rules_env: RuleEnv = HashMap::new();

    match ast::Suite::parse(&python_source, &python_source_path) {
        Ok(ast) => {
            astutils::print_ast(&ast);
            // do one round of type inferrence first,
            // then while types in type_env are not bound,
            // swap between rule inferrence and normal type inferrence?

            // normal type inferrence
            infer_ast_types(&mut type_inferrer, &mut type_env, &ast, &mut type_db);

            println!("final type env: {:?}", type_env);
            compiler.compile(&ast, &type_env);
            // compiler.compile_generically(&ast);
        }
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
