def subscript(x): # should infer that subscript is string
    for i in range(len(x)):
        print(x[i])

# subscript("hi")
subscript([1,2])
# subscript(range(3,5))

x = "hi"
print(x[0])