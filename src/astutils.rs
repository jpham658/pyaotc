use rustpython_parser::ast::{Stmt, StmtFor, StmtFunctionDef, StmtIf, StmtReturn, StmtWhile};

pub fn print_ast(ast: &[Stmt]) {
    for node in ast {
        println!("{:#?}", node);
    }
}

pub trait GetReturnStmts {
    fn get_return_stmts(self) -> Vec<StmtReturn>;
}

impl GetReturnStmts for StmtReturn {
    fn get_return_stmts(self) -> Vec<StmtReturn> {
        return Vec::from([self.clone()])
    }
}

impl GetReturnStmts for StmtFunctionDef {
    fn get_return_stmts(self) -> Vec<StmtReturn> {
        let stmts = self.body.clone();
        let ret_stmts = stmts
            .iter()
            .filter(|stmt| stmt.is_return_stmt())
            .map(|stmt| stmt.as_return_stmt().unwrap().clone())
            .collect::<Vec<_>>();
        ret_stmts
    }
}

impl GetReturnStmts for StmtIf {
    fn get_return_stmts(self) -> Vec<StmtReturn> {
        let mut stmts = self.body.clone();
        stmts.extend(self.orelse.clone());
        let ret_stmts = stmts
            .iter()
            .filter(|stmt| stmt.is_return_stmt())
            .map(|stmt| stmt.as_return_stmt().unwrap().clone())
            .collect::<Vec<_>>();
        ret_stmts
    }
}

impl GetReturnStmts for StmtWhile {
    fn get_return_stmts(self) -> Vec<StmtReturn> {
        let mut stmts = self.body.clone();
        stmts.extend(self.orelse.clone());
        let ret_stmts = stmts
            .iter()
            .filter(|stmt| stmt.is_return_stmt())
            .map(|stmt| stmt.as_return_stmt().unwrap().clone())
            .collect::<Vec<_>>();
        ret_stmts
    }
}

impl GetReturnStmts for StmtFor {
    fn get_return_stmts(self) -> Vec<StmtReturn> {
        let mut stmts = self.body.clone();
        stmts.extend(self.orelse.clone());
        let ret_stmts = stmts
            .iter()
            .filter(|stmt| stmt.is_return_stmt())
            .map(|stmt| stmt.as_return_stmt().unwrap().clone())
            .collect::<Vec<_>>();
        ret_stmts
    }
}
