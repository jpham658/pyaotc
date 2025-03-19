def fib(n):
    a = 0
    b = 1
    while a < n:
        print(a)
        temp = a
        a = b 
        b = temp+b

fib(1000)