# Compiler:
#   status: success
#
# Run-time:
#   status: success
#   stdout: 
#       ...
#       2
#       hello world
#       6
#       6.000000
#       ...
def add(x,y):
    return x + y

print(add(1,1))
print(add("hello ", "world"))
print(add(2,4))
print(add(2.0,4))