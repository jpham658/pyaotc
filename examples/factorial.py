# Compiler:
#   status: success
#
# Run-time:
#   status: error
#   stdout: 
#       ...
#       6
#       6.000000
#       ...
#   
#   stderr:
#       ...
#       Incompatible types for subtraction.
#       ...

def factorial(x):
    # print(x, "==", 0, "?", x == 0)
    if x == 0:
        return 1
    return x * factorial(x-1)
print(factorial(3))
print(factorial(3.0))
print(factorial("wrong string"))