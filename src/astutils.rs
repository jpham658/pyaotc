use rustpython_parser::ast::Stmt;

pub fn print_ast(ast: &[Stmt]) {
    for node in ast {
        println!("{:#?}", node);
    }
}