# Compiler:
#     status: success
#
# Run-time:
#     status: success
#     stdout:
#         ...
#         h 
#         i 
#         1 
#         2 
#         3 
#         4 

def subscript(x): # should infer that subscript is string
    for i in range(len(x)):
        print(x[i])

subscript("hi")
subscript([1,2])
subscript(range(3,5))
