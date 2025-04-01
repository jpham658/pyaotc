# Compiler:
#   status: success
#
# Run-time:
#   status: success
#   stdout: 
#       ...
#       [3, 10, 5, 16, 8, 4, 2, 1] 
#       [3.000000, 10.000000, 5.000000, 16.000000, 8.000000, 4.000000, 2.000000, 1.000000]
#       ...

def collatz(n):
    sequence = [n]
    while n != 1:
        if n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1
        sequence.append(n)
    
    return sequence

print(collatz(3000))
print(collatz(3000.0))