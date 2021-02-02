# Advent of Code 2020, Day 6

import re

with open("../input06", "r") as infile:
     lines = infile.readlines()

group = []
groups = []

def blank(l):
  b = True
  for c in l: b = b and (c == ' ' or c == '\n')
  return b

for l in lines:
  if blank(l):  groups.append(group)
  else: group.append(l[:-1])

# Part 1

def any_ans(g):
  aa = set()
  for l in g: aa.union(set(l))
  return len(aa)

count = 0
for g in groups: count += any_ans(g)
  


print(count)
