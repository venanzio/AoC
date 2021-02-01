# Advent of Code 2020, Day 5

import re

with open("../input05", "r") as infile:
     seats = [l[0:10] for l in infile.readlines()]

def seat_id(seat):
  id = 0
  for d in seat:
    id *= 2
    if d == 'B' or d == 'R': id += 1
  return id

ids = [seat_id(s) for s in seats]

print("Part 1: " + str(max(ids)))
