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

s = P.Source("Sensor at x=2, y=18: closest beacon is at x=-2, y=15")
print(sensor_beacon(s))

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

