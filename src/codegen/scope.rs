use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
};

use inkwell::values::AnyValueEnum;

// Define a pair where the first value is the typed value and the second value is the boxed Object* value
// Both are optional to account for both typed (use first value) and generic compilation (use second value)
type ScopePair<'ctx> = (Option<AnyValueEnum<'ctx>>, Option<AnyValueEnum<'ctx>>);

#[derive(Debug)]
pub struct ScopeManager<'ctx> {
    globals: RefCell<HashMap<String, ScopePair<'ctx>>>,
    scopes: RefCell<VecDeque<HashMap<String, ScopePair<'ctx>>>>,
    in_global_scope: RefCell<bool>,
}

impl<'ctx> ScopeManager<'ctx> {
    pub fn new() -> Self {
        Self {
            globals: RefCell::new(HashMap::new()),
            scopes: RefCell::new(VecDeque::new()),
            in_global_scope: RefCell::new(true),
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.borrow_mut().push_front(HashMap::new());
        self.in_global_scope = RefCell::new(false);
    }

    pub fn exit_scope(&mut self) {
        let mut scopes = self.scopes.borrow_mut();
        scopes.pop_front();
        let in_global_scope = scopes.is_empty();
        self.in_global_scope = RefCell::new(in_global_scope);
    }

    pub fn is_global_scope(&self) -> bool {
        *self.in_global_scope.borrow()
    }

    pub fn add_variable(
        &self,
        name: &str,
        llvm_value: Option<AnyValueEnum<'ctx>>,
        llvm_obj_value: Option<AnyValueEnum<'ctx>>,
    ) {
        if let Some(current_scope) = self.scopes.borrow_mut().front_mut() {
            current_scope.insert(name.to_string(), (llvm_value, llvm_obj_value));
        } else {
            self.globals
                .borrow_mut()
                .insert(name.to_string(), (llvm_value, llvm_obj_value));
        }
    }

    pub fn resolve_variable(&self, name: &str) -> Option<ScopePair<'ctx>> {
        for scope in self.scopes.borrow().iter() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        self.globals.borrow().get(name).cloned()
    }
}
