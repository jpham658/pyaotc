# Compiler:
#   status: success
# Run-time:
#     status: success

start = 1000 
x = start
y = start
z = start
while 0 < x:
    while 0 < y:
        while 0 < z:
            z = z - 1
        z = start
        y = y - 1
    y = start
    x = x - 1