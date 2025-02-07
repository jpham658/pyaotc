def hanoi(n, a, b, c):
    if n == 0:
        return
    hanoi(n-1, a, c, b)
    print(a, "->", b)
    hanoi(n-1, c, b, a)
    
hanoi(4,1,2,3)