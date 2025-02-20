# Compiler:
#   status: success
# Run-time:
#   status: success
#   stdout:
#       ...
#       True
#       True

def dfs(row, col, grid, visited):
    if (row < 0 or row >= len(grid) #row -> int 
       or col < 0 or col >= len(grid[0])    #col -> int
       or visited[row][col] or grid[row][col] == "0"):  #visited -> list[v2], grid[0] -> str
        return
    
    visited[row][col] = True    #v2: bool

    dfs(row - 1, col, grid, visited)
    dfs(row + 1, col, grid, visited)
    dfs(row, col - 1, grid, visited)
    dfs(row, col + 1, grid, visited)

def numIslands(grid) -> "int":
    m = len(grid)   #int
    n = len(grid[0])    #int
    visited: "list[list[bool]]" = []    #list[v0] -> list[list[bool]]
    islandCount = 0 #int
    
    # initialise visited grid
    for i in range(m):  #i -> int
        row: "list[bool]" = []    #list[v1]
        for j in range(n):  #j -> int
            row.append(False)   #v1: bool
        visited.append(row) #v0: list[bool]
    
    for i in range(m):  #i -> int
        for j in range(n):  #j -> int
            if grid[i][j] == "1" and not visited[i][j]:
                islandCount = islandCount + 1
                dfs(i, j, grid, visited) # -> can also infer here that args of dfs should be int -> int -> list[list[str]] -> list[list[bool]]

    return islandCount # int

grid = [
  ["1","1","1","1","0"],
  ["1","1","0","1","0"],
  ["1","1","0","0","0"],
  ["0","0","0","0","0"]
]   # list[list[str]]

grid2 = [
  ["1","1","0","0","0"],
  ["1","1","0","0","0"],
  ["0","0","1","0","0"],
  ["0","0","0","1","1"]
]   # list[list[str]]

print(numIslands(grid) == 1)
print(numIslands(grid2) == 3)