# Advent of Code 2022, day 8
#   Venanzio Capretta

print("Advent of Code 2022, Day 8")

f = open("../input08")
input = f.read().splitlines()
f.close()

# Parsing the input

def num_row(s):
  r = []
  for x in s:
    r.append(int(x))
  return r

grid = [num_row(s) for s in input]

boundy = len(grid)
boundx = len(grid[0])

# Part 1

# a direction is a couple of (dx,dy) increments

def in_bounds(x,y):
  return (0 <= x < boundx and 0 <= y < boundy)

def visible(x,y,dx,dy):
  ix = x+dx
  iy = y+dy
  while in_bounds(ix,iy):
    if grid[y][x] <= grid[iy][ix]:
      return False
  return True

print("Part 1: ")

print(in_bounds(-1,-1))
print(visible(0,0,1,0))

# Part 2

print("Part 2: ")
  

