# Compiler:
#   status: success
# Run-time:
#   status: success
#   stdout:
#       ...
#       1
#       2
#       2
#       3
#       3
#       4
#       4
#       5
#       5
#       6
#       6
#       7

x = [1,2,3,4,5,6]
for i in range(0, len(x)):
    print(x[i])
    x[i] = x[i] + 1
    print(x[i])