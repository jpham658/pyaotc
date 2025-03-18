# Compiler:
#     status: success
#
# Run-time:
#     status: success
#     stdout:
#         ...
#         x: [1, 2, 3]
#         y: [4, 5, 6]
#         x + y: [1, 2, 3, 4, 5, 6]
#         ...

x = [1,2,3]
y = [4,5,6]
print("x:", x)
print("y:", y)
print("x + y:", x + y)