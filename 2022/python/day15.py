# Advent of Code 2022, day 15
#   Venanzio Capretta

import parsers as P 

print("Advent of Code 2022, Day 15")

f = open("../input15")
input = f.read().splitlines()
f.close()

# Parsing the input

def sensor_beacon(s):
  P.word("Sensor at x=",s)
  sx = P.integer(s)
  P.word(", y=",s)
  sy = P.integer(s)
  P.word(": closest beacon is at x=",s)
  bx = P.integer(s)
  P.word(", y=",s)
  by = P.integer(s)
  return (sx,sy,bx,by)

def manhattan(x0,y0,x1,y1):  #Manhattan distance
  return abs(x0-x1)+abs(y0-y1)

sensors = set()
beacons = set()
for l in input:
  s = P.Source(l)
  sx,sy,bx,by = sensor_beacon(s)
  sensors.add((sx,sy,manhattan(sx,sy,bx,by)))
  beacons.add((bx,by))

# Part 1

def merge_ranges(l0,h0,l1,h1):
  if h0<l1 or h1<l0:   # no overlap
    return None
  else:
    return (min(l0,l1),max(h0,h1))

rowy = 2000000
nob_ranges = []   # ranges
for (sx,sy,d) in sensors:
  radius = d-abs(sy-rowy)
  if radius >=0 :
    l,h = sx-radius, sx+radius
    for (l0,h0) in nob_ranges:
      mr = merge_ranges(l0,h0,l,h)
      if mr == None
...

no_beacon = 0
for (l,h) in nob_ranges:
  no_beacon += h-l+1

for (bx,by) in beacons:
  if by == rowy:
    no_beacon -= 1

print("Part 1: ")
print(no_beacon)

# Part 2

print("Part 2: ")
  

