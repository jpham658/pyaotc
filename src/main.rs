use exprrules::Compiler;
use exprrules::LLVMCodeGen;
use inkwell::context::Context;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use rustpython_parser::{ast, Parse};
mod astutils;
mod exprrules;

fn main() {
    //     let python_source = r#"
    // x = 3
    // x + 4.0
    // "#;
    let python_source = r#"
3 + 4
"#;
    let context = Context::create();
    let compiler = Compiler::new(&context);

    match ast::Suite::parse(&python_source, "<embedded>") {
        Ok(ast) => {
            let i64_type = compiler.context.i64_type();
            let main = compiler.module.add_function(
                "main", 
                i64_type.fn_type(&[], false), 
                None
            );
            let main_entry = compiler.context.append_basic_block(main, "entry");
            compiler.builder.position_at_end(main_entry);

            let mut last_val = None;

            for statement in ast {
                match statement.codegen(&compiler) {
                    Ok(ir) => {
                        last_val = Some(ir); // Store last evaluated IR value
                    },
                    Err(e) => {
                        println!("{:?}", e);
                        return;
                    }
                }
            }

            if let Some(final_val) = last_val {
                // Return the final evaluated value, assuming it’s a float to cast to i64
                match final_val {
                    AnyValueEnum::FloatValue(float_val) => {
                        compiler.builder.build_return(Some(&float_val));
                    },
                    AnyValueEnum::IntValue(int_val) => {
                        compiler.builder.build_return(Some(&int_val));
                    },
                    _ => {}
                }
            }

            compiler.dump_module();
        },
        Err(e) => {
            eprintln!("ParseError: {}", e);
        }
    };
}
