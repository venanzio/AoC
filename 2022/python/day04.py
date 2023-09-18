# Advent of Code 2022, day 4
#   Venanzio Capretta

print("Advent of Code 2022, Day 4")

f = open("../input04")
input = f.read().strip()
f.close()

# Parsing the input

def parsePair(s):
  vs = s.replace('-',',').split(',')
  return ((vs[0],vs[1]),(vs[2],vs[3]))

pairRange = list(map(parsePair,input.splitlines()))

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

