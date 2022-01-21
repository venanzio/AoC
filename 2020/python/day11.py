# Advent of Code 2020, Day 11

import re

f = open("../input")
input = f.read().strip()
f.close()

# Parsing the input

seats = { }
i = 0
j = 0
for c in input:
  if c in "L#.":
    seats.update({(i,j):c})
    j += 1
  else:
    i +=1
    j = 0

seats2 = seats.copy() # backup for part 2

def showSeats(seats):
  s = ""
  (l,w) = max(seats.keys())
  for i in range(0,l+1):
    for j in range(0,w+1):
      s += seats[(i,j)]
    s += '\n'
  return s


# Part 1

directions = [(u,v) for u in [-1,0,1] for v in [-1,0,1] if (u,v) != (0,0)]

# all depends on a function that returns the coordinates of a position in a given direction
# its just the first position in that direction for part 1, first seat in that direction for part 2
def dirN(i,j,u,v):
  return (i+u,j+v)

# how many occupied seats are visible?
def views(dirV,i,j):
  count = 0
  for (u,v) in directions:
    (w,z) = dirV(i,j,u,v)
    if (w,z) in seats and seats[(w,z)]=='#': count += 1
  return count

# dictionary of neighbour numbers
# it's important to calculate all the numberse before modifying seats
nHood = { }
def genN(dirV):
  for (i,j) in seats:
    nHood.update({(i,j):views(dirV,i,j)})

# compute the next generation: return True if there has been changed
def generation(dirV,maxOccupied):
  changed = False
  genN(dirV)
  for (i,j) in seats:
      if seats[(i,j)]=='L' and nHood[(i,j)]==0: 
        seats[(i,j)]='#'
        changed = True
      elif seats[(i,j)]=='#' and nHood[(i,j)]>=maxOccupied:
        seats[(i,j)]='L'
        changed = True
  return changed

def countOccupied():
  s = 0
  for c in seats.values():
    if c=='#': s+=1
  return s

# keep repeating generations until there is no change
def finalOccupied(dirV,maxOccupied):
  while generation(dirV,maxOccupied): continue
  return countOccupied()

print("Part 1: " + str(finalOccupied(dirN,4)))

# Part 2

seats = seats2.copy()

# first seat in direction (u,v) from (i,j)
def dirView(i,j,u,v):
  i += u
  j += v
  while (i,j) in seats and seats[(i,j)] == '.':
    i += u
    j += v
  return (i,j)

# print(showSeats(seats))

print("Part 2: " + str(finalOccupied(dirView,5)))

  

