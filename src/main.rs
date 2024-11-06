use rustpython_parser::{ast, Parse};
mod astutils;
mod varrules;

fn main() {
    //     let python_source = r#"
    // x = 3
    // x + 4.0
    // "#;
    let python_source = r#"
x + 4.0
"#;

    match ast::Suite::parse(python_source, "<embedded>") {
        Ok(ast) => {
            astutils::print_ast(&ast);
            let map = varrules::apply_rules(&ast);
            println!("{:?}", map);       
        }
        Err(e) => eprintln!("Error parsing code: {:?}", e),
    }
}
