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

(length, width) = max(seats.keys())
length += 1
width += 1

def showSeats():
  s = ""
  for i in range(0,length):
    for j in range(0,width):
      s += seats[(i,j)]
    s += '\n'
  return s

# Part 1

# coordinates of neighbours
def nCoords(i,j):
  return [(i+u,j+v) for u in [-1,0,1] for v in [-1,0,1]
                    if (u,v) != (0,0) and (i+u,j+v) in seats
         ]

# how many neighbours are occupied?
def neighbours(i,j):
  count = 0
  for (u,v) in nCoords(i,j):
    if seats[(u,v)]=='#': count += 1
  return count

# dictionary of neighbour numbers
# it's important to calculate all the numberse before modifying seats
nHood = { }
def genN():
  for (i,j) in seats:
    nHood.update({(i,j):neighbours(i,j)})

# compute the next generation: return True if there has been changed
def generation():
  changed = False
  genN()
  for (i,j) in seats:
      if seats[(i,j)]=='L' and nHood[(i,j)]==0: 
        seats[(i,j)]='#'
        changed = True
      elif seats[(i,j)]=='#' and nHood[(i,j)]>=4:
        seats[(i,j)]='L'
        changed = False

while generation(): continue

print("Part 1: ")


# Part 2

print("Part 2: ")
  

