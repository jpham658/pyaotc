# Compiler:
#   status: success
# Run-time:
#   status: success
#   stdout:
#       ...
#       1
#       2
#       3
#       4
#       5
#       6

x = [1,2,3,4,5,6]
for i in range(0, len(x)):
    print(x[i]) # should print items at each index