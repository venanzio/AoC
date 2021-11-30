# Advent of Code 2020, Day 3

with open("../input03", "r") as infile:
     input = infile.readlines()

# divide the input into lines
lines = [i.split("\n")[0] for i in input]

n = len(lines[0])
m = len(lines)

# count the trees along a slope, from the top-right corner
def countTrees (dx,dy):
  x = 0
  y = 0
  trs = 0
  while y < m:
    if lines[y][x] == '#': trs+=1
    x = (x+dx) % n
    y = y+dy
  return(trs)

# Solutions

print('Part 1: ' + str(countTrees(3,1)))
print('Part 2: ' + str(countTrees (1,1) *
                       countTrees (3,1) *
                       countTrees (5,1) *
                       countTrees (7,1) *
                       countTrees (1,2)))
