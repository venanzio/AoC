# Advent of Code 2022, day 4
#   Venanzio Capretta

print("Advent of Code 2022, Day 4")

f = open("../input04")
input = f.read().strip()
f.close()

# Parsing the input

def parsePair(s):
  rs = input.split(',')
  r0 = rs[0].split('-')
  r1 = rs[1].split('-')
  return((int(r0[0]),int(r0[1])),(int(r1[0]),int(r1[1])))

# pairRange = map(parsePair,input.splitlines())

# print(list(pairRange))

print(parsePair('1-6,12-30'))

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

