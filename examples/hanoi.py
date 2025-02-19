# Compiler:
#   status: success
#
# Run-time:
#   status: success
#   stdout: 
#       ...
#       1 -> 3 
#       1 -> 2 
#       3 -> 2 
#       1 -> 3 
#       2 -> 1 
#       2 -> 3 
#       1 -> 3 
#       1 -> 2 
#       3 -> 2 
#       3 -> 1 
#       2 -> 1 
#       3 -> 2 
#       1 -> 3 
#       1 -> 2 
#       3 -> 2 
#       str -> True 
#       str -> 2.000000 
#       True -> 2.000000 
#       str -> True 
#       2.000000 -> str 
#       2.000000 -> True 
#       str -> True 
#       str -> 2.000000 
#       True -> 2.000000 
#       True -> str 
#       2.000000 -> str 
#       True -> 2.000000 
#       str -> True 
#       str -> 2.000000 
#       True -> 2.000000 

def hanoi(n, a: int, b: int, c: int):
    if n == 0:
        return
    hanoi(n-1, a, c, b)
    print(a, "->", b)
    hanoi(n-1, c, b, a)
    
hanoi(4,1,2,3)
hanoi(4,"str",2.0,True)