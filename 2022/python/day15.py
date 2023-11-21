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

def nob_range(rowy):
  nobr = []
  new_nobr = [] 
  for (sx,sy,d) in sensors:
    radius = d-abs(sy-rowy)
    if radius >= 0 :
      l,h = sx-radius, sx+radius
      new_nobr = []
      for (l0,h0) in nobr:
        mr = merge_ranges(l0,h0,l,h)
        if mr == None:
          new_nobr.append((l0,h0))
        else:
          l,h = mr[0],mr[1]
      nobr = new_nobr.copy()
      nobr.append((l,h))
  return nobr


rowy = 2000000

# compute the size of all ranges
no_beacon = 0
for (l,h) in nob_range(rowy):
  no_beacon += h-l+1

# eliminate the positions with beacons
for (bx,by) in beacons:
  if by == rowy:
    no_beacon -= 1

print("Part 1: ")
print(no_beacon)

# Part 2

lx,hx = 0,4000000

def not_in_range(range):
  l,h = lx,hx
  for (l0,h0) in range:
    if l<l0:
      return l
    elif h<=h0:
      return None
    else:
      l = max(l,h0+1)
  return l

y = -1
x = None
while x == None:
  y += 1
  range = nob_range(y)
  range.sort()
  x = not_in_range(range)

print("Part 2: ")
print(x*4000000+y)  

