# Compiler:
#   status: success
# Run-time:
#   status: success
#   stdout:
#       ...
#       5.0

def add(x):
    return x + 1

y = add(2.0) # add(x) is inferred to have type int -> int
z = y + 2
print(z)
