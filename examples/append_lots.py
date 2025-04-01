# Compiler:
#   status: success
#
# Run-time:
#   status: success

def benchmark_large_appends():
    lst = []
    
    for i in range(100000): 
        lst.append(i)

benchmark_large_appends()
