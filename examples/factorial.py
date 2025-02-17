# Compiler:
#   stderr:
#       ...
#       Invalid operand types for multiplication.
#       ...
#
# Run-time:
#   stdout: 6\n6.000000
def factorial(x):
    if x == 0:
        return 1
    return x * factorial(x-1)
print(factorial(3))
print(factorial(3.0))