# Advent of Code 2022, day 4
#   Venanzio Capretta

print("Advent of Code 2022, Day 4")

f = open("../input04")
input = f.read().strip()
f.close()

# Parsing the input

def parsePair(s):
  vs = s.replace('-',',').split(',')
  return ((int(vs[0]),int(vs[1])),(int(vs[2]),int(vs[3])))

pairRange = list(map(parsePair,input.splitlines()))

# Part 1

def rangeContained(r0,r1):
  return(r0[0] >= r1[0] and r0[1] <= r1[1])

contained = 0
for (r0,r1) in pairRange:
  if rangeContained(r0,r1) or rangeContained(r1,r0):
    contained += 1

print("Part 1: ")
print(contained)

# Part 2

def overlap(r0,r1):
  return(r0[0]<=r1[1] and r1[0]<=r0[1])

overlapping = 0
for (r0,r1) in pairRange:
  if overlap(r0,r1):
    overlapping += 1

print("Part 2: ")
print(overlapping)

