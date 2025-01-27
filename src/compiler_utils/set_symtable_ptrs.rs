use std::collections::HashMap;

use inkwell::values::AnyValueEnum;

use crate::compiler::Compiler;

/**
 * For all variables in the input hashmap, change the symbol table
 * so that it has a new pointer.
 * Used to change the symbol table temporarily to handle Any types 
 */
pub fn set_variable_pointers_in_symbol_table<'a>(
    compiler: &Compiler<'a>,
    new_var_ptrs: &HashMap<String, AnyValueEnum<'a>>,
) {
    let mut sym_table = compiler.sym_table.borrow_mut();
    for var in sym_table.keys().cloned().collect::<Vec<String>>() {
        if let Some(value) = sym_table.get_mut(&var) {
            *value = new_var_ptrs[&var];
        }
    }
}
