# Compiler:
#     status: success
#
# Run-time:
#     status: success
#     stdout:
#         ...
#         [0, 2, 4, 6]
#         [0, 2, 4, 6]
#         ...

def f(x):
    l = []
    for i in range(len(x)):
      l.append(x[i] + i)
    return l

print(f([0,1,2,3]))
print(f(range(0,4)))