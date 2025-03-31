# Compiler:
#   status: error
#   stderr:
#       ...
#       BackendError { message: "Variable b is not defined." }

a = 3

def foo():
  return a + b

b = 3

print(foo())