# Compiler:
#   status: error
#   stderr:
#       ...
#       BackendError { message: "Function {func_name} not typed." }

a = 3

def foo():
  return a + b

b = 3

print(foo())