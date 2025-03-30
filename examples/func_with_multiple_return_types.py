# Compiler:
#     status: error
#     stderr: 
#       ...
#       BackendError { message: "Functions with more than one return type are not supported." }
#       ...

def f(x):
    if x == 0:
        return "hi"
    else:
        return 1