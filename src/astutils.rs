use rustpython_parser::ast::{
    Expr, ExprSubscript, Stmt, StmtFor, StmtFunctionDef, StmtIf, StmtReturn, StmtWhile,
};

use crate::type_inference::{ConcreteValue, NodeTypeDB, Type};

pub fn print_ast(ast: &[Stmt]) {
    for node in ast {
        println!("{:#?}", node);
    }
}

/**
 * Helper to serialise a subscript node
 */
pub fn serialise_subscript(subscript: &ExprSubscript) -> String {
    let value_string = if let Some(name) = subscript.value.as_name_expr() {
        name.id.as_str().to_string()
    } else if let Some(val_subscript) = subscript.value.as_subscript_expr() {
        serialise_subscript(val_subscript)
    } else {
        "".to_string()
    };

    format!("{value_string}[]")
}

/**
 * Get type name of the given node
 */
pub fn get_iter_type_name(node: &Expr, types: &NodeTypeDB) -> String {
    // check if node is a name
    if let Some(name) = node.as_name_expr() {
        let type_name = match types.get(&name.range) {
            Some(typ) => match typ {
                Type::Range => "range",
                Type::List(..) => "list",
                Type::Mapping(_, _) => "dict",
                Type::ConcreteType(ConcreteValue::Str) => "str",
                _ => "",
            },
            _ => "",
        };

        return type_name.to_string();
    }

    if let Some(call) = node.as_call_expr() {
        if !call.func.is_name_expr() {
            return "".to_string();
        }

        let call_name = call.func.as_name_expr().unwrap().id.as_str();
        if call_name.eq("range") {
            return "range".to_string();
        } else {
            return "".to_string();
        }
    }

    let type_name = if node.is_list_expr() {
        "list"
    } else if node.is_dict_expr() {
        "dict"
    } else if node.is_set_expr() {
        "set"
    } else if node.is_constant_expr() && node.as_constant_expr().unwrap().value.is_str() {
        "str"
    } else {
        ""
    };

    type_name.to_string()
}

pub trait GetReturnStmts {
    fn get_return_stmts(self) -> Vec<StmtReturn>;
}

impl GetReturnStmts for StmtReturn {
    fn get_return_stmts(self) -> Vec<StmtReturn> {
        return Vec::from([self.clone()]);
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
