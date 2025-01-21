; ModuleID = 'python_module'
source_filename = "python_module"

@format_string = private unnamed_addr constant [5 x i8] c"%d \0A\00", align 1

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  ; cast Int to Any
  %x = alloca { i8, [8 x i8] }
  %0 = getelementptr inbounds { i8, [8 x i8] }, { i8, [8 x i8] }* %x, i32 0, i32 0
  store i8 1, i8* %0
  %1 = bitcast { i8, [8 x i8] }* %x to { i8, i64 }*
  %2 = getelementptr inbounds { i8, i64 }, { i8, i64 }* %1, i32 0, i32 1
  store i64 30, i64* %2

  ; cast Any to Int (if it is)
  %3 = getelementptr inbounds { i8, [8 x i8] }, { i8, [8 x i8] }* %x, i32 0, i32 0
  %4 = load i8, i8* %3
  %5 = bitcast { i8, [8 x i8] }* %x to { i8, i64 }*
  %6 = getelementptr inbounds { i8, i64 }, { i8, i64 }* %5, i32 0, i32 1
  %7 = load i64, i64* %6 ; load value of Any container
  ret i32 0
}