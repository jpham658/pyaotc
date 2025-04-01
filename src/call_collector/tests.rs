use crate::call_collector::*;
use crate::type_inference::{NodeTypeDB, Type};
use rustpython_parser::{
    ast::{self},
    Parse,
};
use std::collections::HashMap;

#[cfg(test)]
mod call_collector_tests {
    use rustpython_ast::Constant;
    use crate::type_inference::ConcreteValue;
    use super::*;

    fn parse_python_code(code: &str) -> Vec<Stmt> {
        ast::Suite::parse(code, "<embedded>").expect("Failed to parse Python code")
    }

    fn convert_expr_to_type(expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Name(..) => {
                Some(Type::ConcreteType(ConcreteValue::Int))
            }
            Expr::Constant(constant) => match constant.value {
                Constant::Bool(..) => Some(Type::ConcreteType(ConcreteValue::Bool)),
                Constant::Int(..) => Some(Type::ConcreteType(ConcreteValue::Int)),
                Constant::Float(..) => Some(Type::ConcreteType(ConcreteValue::Float)),
                Constant::Str(..) => Some(Type::ConcreteType(ConcreteValue::Str)),
                _ => None,
            },
            Expr::List(list) => {
                // Assumes given list has more than one element
                let elt_type = convert_expr_to_type(&list.elts[0]);
                if let Some(typ) = elt_type {
                    Some(Type::List(Box::new(typ)))
                } else { 
                    None
                }
            }
            _ => None,
        }
    }

    fn populate_type_db(type_db: &mut NodeTypeDB, ast: &[Stmt]) {
        for stmt in ast {
            match stmt {
                Stmt::Expr(expr_stmt) => {
                    if let Expr::Call(call) = &*expr_stmt.value {
                        for arg in &call.args {
                            let arg_type = convert_expr_to_type(arg).unwrap();
                            type_db.insert(arg.range(), arg_type);
                        }
                    }
                }
                Stmt::If(ifstmt) => {
                    let test_type = convert_expr_to_type(&ifstmt.test).unwrap();
                    type_db.insert(ifstmt.test.range(), test_type);
                    populate_type_db(type_db, &ifstmt.body);
                    populate_type_db(type_db, &ifstmt.orelse);
                }
                Stmt::While(whilestmt) => {
                    let test_type = convert_expr_to_type(&whilestmt.test).unwrap();
                    type_db.insert(whilestmt.test.range(), test_type);
                    populate_type_db(type_db, &whilestmt.body);
                    populate_type_db(type_db, &whilestmt.orelse);
                }
                Stmt::For(forstmt) => {
                    let target_type = convert_expr_to_type(&forstmt.target).unwrap();
                    type_db.insert(forstmt.target.range(), target_type);
                    populate_type_db(type_db, &forstmt.body);
                    populate_type_db(type_db, &forstmt.orelse);
                }
                _ => {}
            }
        }
    }

    #[test]
    fn test_function_calls_are_recorded() {
        let test_program = "def foo(x): return x\nfoo(1)\nfoo(2)";
        let ast = parse_python_code(test_program);
        let mut type_db = NodeTypeDB::new();
        populate_type_db(&mut type_db, &ast);

        let mut collector = FunctionCallCollector::new(&type_db);
        collector.collect_calls(&ast);

        let stats = &collector.call_stat_tracker.stats;
        let expected_type_combo =
            HashMap::from([(Vec::from([Type::ConcreteType(ConcreteValue::Int)]), 2)]);
        let actual_type_combo = stats.get("foo").unwrap();
        assert_eq!(actual_type_combo.len(), 1);
        assert_eq!(*actual_type_combo, expected_type_combo);
    }

    #[test]
    fn test_different_argument_types() {
        let test_program = "def bar(x): return x\nbar(1)\nbar('hello')";
        let ast = parse_python_code(test_program);
        let mut type_db = NodeTypeDB::new();
        populate_type_db(&mut type_db, &ast);

        let mut collector = FunctionCallCollector::new(&type_db);
        collector.collect_calls(&ast);

        let stats = &collector.call_stat_tracker.stats;
        let expected_type_combo = HashMap::from([
            (Vec::from([Type::ConcreteType(ConcreteValue::Int)]), 1),
            (Vec::from([Type::ConcreteType(ConcreteValue::Str)]), 1),
        ]);
        let actual_type_combo = stats.get("bar").unwrap();
        assert_eq!(actual_type_combo.len(), 2);
        assert_eq!(*actual_type_combo, expected_type_combo);
    }

    #[test]
    fn test_most_common_argument_type() {
        let test_program = "def baz(x): return x\nbaz(1)\nbaz(2)\nbaz('hello')";
        let ast = parse_python_code(test_program);
        let mut type_db = NodeTypeDB::new();
        populate_type_db(&mut type_db, &ast);

        let mut collector = FunctionCallCollector::new(&type_db);
        collector.collect_calls(&ast);

        let stats = &collector.call_stat_tracker.stats;
        let expected_type_combo = HashMap::from([
            (Vec::from([Type::ConcreteType(ConcreteValue::Int)]), 2),
            (Vec::from([Type::ConcreteType(ConcreteValue::Str)]), 1),
        ]);
        let actual_type_combo = stats.get("baz").unwrap();
        assert_eq!(actual_type_combo.len(), 2);
        assert_eq!(*actual_type_combo, expected_type_combo);

        let actual_most_common_types = collector.most_common_arg_types();
        let expected_most_common_types = HashMap::from([
            ("baz".to_string(), Vec::from([Type::ConcreteType(ConcreteValue::Int)]))
        ]);
        assert_eq!(actual_most_common_types, expected_most_common_types);
    }

    #[test]
    fn test_builtin_functions_are_ignored() {
        let test_program = "print('hello')\nx = len([1,2,3])\nfor i in range(5): return x";
        let ast = parse_python_code(test_program);
        let mut type_db = NodeTypeDB::new();
        populate_type_db(&mut type_db, &ast);

        let mut collector = FunctionCallCollector::new(&type_db);
        collector.collect_calls(&ast);

        let stats = &collector.call_stat_tracker.stats;
        assert!(stats.is_empty());
    }

    #[test]
    fn test_nested_function_calls() {
        let test_program = "def foo(x): return x\nfor i in range(3): foo(i)";
        let ast = parse_python_code(test_program);
        let mut type_db = NodeTypeDB::new();
        populate_type_db(&mut type_db, &ast);

        let mut collector = FunctionCallCollector::new(&type_db);
        collector.collect_calls(&ast);

        let stats = &collector.call_stat_tracker.stats;
        let expected_type_combo =
            HashMap::from([(Vec::from([Type::ConcreteType(ConcreteValue::Int)]), 1)]);
        let actual_type_combo = stats.get("foo").unwrap();
        assert_eq!(actual_type_combo.len(), 1);
        assert_eq!(*actual_type_combo, expected_type_combo);
    }
}
