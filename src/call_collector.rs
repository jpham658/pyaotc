use std::collections::HashMap;

use rustpython_ast::{Expr, ExprCall, Ranged, Stmt, Visitor};

use crate::type_inference::{NodeTypeDB, Type};

struct FunctionCallStats {
    stats: HashMap<String, HashMap<Vec<Type>, usize>>,
}

impl FunctionCallStats {
    fn new() -> Self {
        Self {
            stats: HashMap::new(),
        }
    }

    fn record_call(&mut self, func_name: String, arg_types: Vec<Type>) {
        let func_stats = self.stats.entry(func_name).or_insert_with(HashMap::new);
        *func_stats.entry(arg_types).or_insert(0) += 1;
    }
}

pub struct FunctionCallCollector {
    call_stat_tracker: FunctionCallStats,
    type_db: NodeTypeDB,
}

impl FunctionCallCollector {
    pub fn new(type_db: &NodeTypeDB) -> Self {
        Self {
            call_stat_tracker: FunctionCallStats::new(),
            type_db: type_db.clone(),
        }
    }

    pub fn collect_calls(&mut self, ast: &[Stmt]) {
        for stmt in ast {
            self.visit_stmt(stmt.clone());
        }
    }

    pub fn most_common_arg_types(&self) -> HashMap<String, Vec<Type>> {
        self.call_stat_tracker
            .stats
            .iter()
            .map(|(func_name, arg_combinations)| {
                let mut arg_count: Vec<_> = arg_combinations.iter().collect();
                arg_count.sort_by(|a, b| b.1.cmp(a.1));
                let most_common = arg_count[0].0;
                (func_name.clone(), most_common.clone())
            })
            .collect::<HashMap<String, Vec<Type>>>()
    }
}

impl Visitor for FunctionCallCollector {
    fn visit_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::FunctionDef(func_def) => {
                // Visit the function body
                for body_stmt in &func_def.body {
                    self.visit_stmt(body_stmt.clone());
                }
            }
            Stmt::Assign(assign) => {
                // Visit the value being assigned
                self.visit_expr(*assign.value.clone());
            }
            Stmt::Expr(expr_stmt) => {
                // Visit the expression
                self.visit_expr(*expr_stmt.value.clone());
            }
            Stmt::If(if_stmt) => {
                // Visit the condition, then the body, and finally the else block
                self.visit_expr(*if_stmt.test.clone());
                for body_stmt in &if_stmt.body {
                    self.visit_stmt(body_stmt.clone());
                }
                for else_stmt in &if_stmt.orelse {
                    self.visit_stmt(else_stmt.clone());
                }
            }
            Stmt::For(for_stmt) => {
                // Visit the iterable, then the body
                self.visit_expr(*for_stmt.iter.clone());
                for body_stmt in &for_stmt.body {
                    self.visit_stmt(body_stmt.clone());
                }
            }
            Stmt::While(while_stmt) => {
                // Visit the condition, then the body
                self.visit_expr(*while_stmt.test.clone());
                for body_stmt in &while_stmt.body {
                    self.visit_stmt(body_stmt.clone());
                }
            }
            _ => {
                // Handle other statement types
                self.generic_visit_stmt(stmt);
            }
        }
    }

    fn visit_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Call(call) => self.visit_expr_call(call),
            Expr::Subscript(subscript) => {
                self.visit_expr(*subscript.value.clone());
                self.visit_expr(*subscript.slice.clone());
            }
            Expr::Attribute(attr) => {
                self.visit_expr(*attr.value.clone());
            }
            Expr::BinOp(bin_op) => {
                self.visit_expr(*bin_op.left.clone());
                self.visit_expr(*bin_op.right.clone());
            }
            Expr::UnaryOp(unary_op) => {
                self.visit_expr(*unary_op.operand.clone());
            }
            _ => {
                self.generic_visit_expr(expr);
            }
        }
    }

    fn visit_expr_call(&mut self, call: ExprCall) {
        if !call.func.is_name_expr() {
            return;
        }

        let func_as_name = call.func.as_name_expr().unwrap();
        let func_name = func_as_name.id.as_str();

        if func_name.eq("print") || func_name.eq("len") || func_name.eq("range") {
            for arg in &call.args {
                self.visit_expr(arg.clone());
            }
            return;
        }

        let arg_types = call
            .args
            .iter()
            .map(|arg| {
                println!("at node {:?}", &arg.range());
                match self.type_db.get(&arg.range()).unwrap().clone() {
                    Type::Scheme(scheme) => *scheme.type_name.clone(),
                    typ => typ,
                }
            })
            .collect();

        self.call_stat_tracker
            .record_call(func_name.to_string(), arg_types);
    }
}
