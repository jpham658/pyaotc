# Compiler:
#     status: success
#
# Run-time:
#     status: success
#     stdout:
#         ...
#         h
#         i
#         !
#         1
#         2
#         3
#         4
#         ...

def iterate(x): # should infer str -> None
    for i in x:
        print(i)

iterate("hi!")
iterate([1,2])
iterate(range(3,5))