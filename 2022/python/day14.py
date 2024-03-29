# Advent of Code 2022, day 14
#   Venanzio Capretta

import parsers as P
print("Advent of Code 2022, Day 14")

f = open("../input14")
input = [P.Source(l) for l in f.read().splitlines()]
f.close()

# Parsing the input

def rock_line(p1,p2):
  if p1[0]==p2[0]:
    st = min(p1[1],p2[1])
    en = max(p1[1],p2[1])
    return {(p1[0],x) for x in range(st,en+1)}
  elif p1[1]==p2[1]:
    st = min(p1[0],p2[0])
    en = max(p1[0],p2[0])
    return {(x,p1[1]) for x in range(st,en+1)}
  else:
    return None

def rock_path(s):
  rks = P.lst_sep(lambda s: P.pair(P.num,P.num,s), '->',s)
  r0 = rks[0]
  path = { r0 }
  for r1 in rks[1:]:
    path = path.union(rock_line(r0,r1))
    r0 = r1
  return path

rock_map = set()
for p in input:
  rock_map = rock_map.union(rock_path(p))

# Part 1

floor = max([y for (x,y) in rock_map])+2

def sand_fall(x,y):
  u,v = x,y
  while v < floor-1:
    if (u,v+1) in rock_map:
      if (u-1,v+1) in rock_map:
        if (u+1,v+1) in rock_map:
          return (u,v)  # come to rest
        else:
          u += 1
          v += 1
      else:
          u -= 1
          v += 1
    else:
      v +=1
  return (u,v)


sand_grain = 0
(x,y) = sand_fall(500,0)
while y < floor-1:
  rock_map.add((x,y))
  sand_grain += 1
  (x,y) = sand_fall(500,0)


print("Part 1: ")
print(sand_grain)

# Part 2

sand_grain += 1
while y > 0:
  rock_map.add((x,y))
  sand_grain += 1
  (x,y) = sand_fall(500,0)


print("Part 2: ")
print(sand_grain)

