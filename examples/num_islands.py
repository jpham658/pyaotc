# Compiler:
#   status: success
# Run-time:
#   status: success
#   stdout:
#       ...
#       True
#       True

def dfs(row, col, grid, visited):
    if (row < 0 or row >= len(grid) 
       or col < 0 or col >= len(grid[0])
       or visited[row][col] or grid[row][col] == "0"):
        return
    
    visited[row][col] = True

    dfs(row - 1, col, grid, visited)
    dfs(row + 1, col, grid, visited)
    dfs(row, col - 1, grid, visited)
    dfs(row, col + 1, grid, visited)

def numIslands(grid) -> int:
    m = len(grid)
    n = len(grid[0])
    visited = []
    islandCount = 0
    
    # initialise visited grid
    for i in range(m):
        row = []
        for j in range(n):
            row.append(False)
        visited.append(row)
    
    for i in range(m):
        for j in range(n):
            if grid[i][j] == "1" and not visited[i][j]:
                islandCount = islandCount + 1
                dfs(i, j, grid, visited)

    return islandCount

grid = [
  ["1","1","1","1","0"],
  ["1","1","0","1","0"],
  ["1","1","0","0","0"],
  ["0","0","0","0","0"]
]

grid2 = [
  ["1","1","0","0","0"],
  ["1","1","0","0","0"],
  ["0","0","1","0","0"],
  ["0","0","0","1","1"]
]

print(numIslands(grid) == 1)
print(numIslands(grid2) == 3)