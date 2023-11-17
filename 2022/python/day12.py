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

def height(i,j):
  c = input[i][j]
  if c == 'S':
    return ord('a')
  elif c == 'E':
    return ord('z')
  else:
    return ord(c)

gr = { }
for i in range(0,len(input)):
  for j in range(0,len(input[i])):
    if input[i][j] == 'S':
      S = (i,j)
    elif input[i][j] == 'E':
      E = (i,j)
    gr[(i,j)] = [(i0,j0) for (i0,j0) in neighbours(i,j) 
                         if height(i0,j0)+1 >= height(i,j)]


# Part 1

print("Part 1: ")
print(dijkstra.shortest(gr,E,S))

# Part 2

print("Part 2: ")
  

