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
    ix += dx
    iy += dy
  return True

vis_trees = 0
for x in range(0,boundx):
  for y in range(0,boundy):
    if visible(x,y,-1,0) or visible(x,y,+1,0) or visible(x,y,0,-1) or visible(x,y,0,+1):
      vis_trees += 1

print("Part 1: ")
print(vis_trees)

# Part 2

def visible_from(x,y,dx,dy):
  ix = x+dx
  iy = y+dy
  d = 0
  while in_bounds(ix,iy) and grid[y][x] > grid[iy][ix]:
    d += 1
    ix += dx
    iy += dy
  if in_bounds(ix,iy):
    d += 1
  return d

def scenic_score(x,y):
  return (visible_from(x,y,-1,0) * visible_from(x,y,+1,0) * visible_from(x,y,0,-1) * visible_from(x,y,0,+1))

print("Part 2: ")
print(max([scenic_score(x,y) for x in range(0,boundx) for y in range(0,boundy)]))
