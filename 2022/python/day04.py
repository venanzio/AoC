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

def rangeContained(r1,r2):
  return(r1[0]>=r2[0] and r1[1]<=r2[1])

contained = list(filter(lambda rs: rangeContained(rs[0],rs[1]) or rangeContained(rs[1],rs[0]),
                        pairRange))

print("Part 1: ")
print(len(contained))

# Part 2

print("Part 2: ")
  

