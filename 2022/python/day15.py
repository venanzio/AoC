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

print(sensors)
print(beacons)

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

