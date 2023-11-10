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

# Part 1

# a direction is a couple of (dx,dy) increments

def visible(x,y,dx,dy)
  ix = x+dx
  iy = y+dy
  while not is_None(grid[iy][ix]):
    if grid[y][x] <= grid[iy][ix]:
      return False
  return True

print("Part 1: ")



# Part 2

print("Part 2: ")
  

