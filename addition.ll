; ModuleID = 'addition_module'
source_filename = "addition_module"

define i64 @add() {
entry:
  ret i64 7
}

define i32 @main() {
entry:
  %call_add = call i64 @add()
  %cast = trunc i64 %call_add to i32
  ret i32 %cast
}
