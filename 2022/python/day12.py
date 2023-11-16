# Advent of Code 2022, day 12
#   Venanzio Capretta

import dijkstra

print("Advent of Code 2022, Day 12")

f = open("../input12")
input = f.read().splitlines()
f.close()

# Parsing the input

n = len(input)
m = len(input[0])

def neighbours(i,j):
  return {(i-1,j),(i+1,j),(i,j-1),(i,j+1)}.difference({(-1,j),(n,j),(i,-1),(i,m)})

gr = { }
for i in range(0,len(input)):
  for j in range(0,len(input[i])):
    gr[(i,j)] = [(i0,j0) in [(i-1



# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

