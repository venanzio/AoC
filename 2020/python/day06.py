# Advent of Code 2020, Day 6

with open("../input06", "r") as infile:
     lines = infile.readlines()

# check if a line is blank
def blank(l):
  b = True
  for c in l: b = b and (c == ' ' or c == '\n')
  return b

group = []
groups = []

for l in lines:
  if blank(l):  
    groups.append(group)
    group = []
  else: 
   group.append(l[:-1])
groups.append(group)

# Part 1

def any_ans(g):
  aa = set()
  for l in g: aa.update(set(l))
  return len(aa)

count = 0
for g in groups: count += any_ans(g)

print("Part 1: " + str(count))

# Part 2

def all_ans(g):
  aa = set([chr(c) for c in range(ord('a'),ord('z')+1)])
  for l in g: aa.intersection_update(set(l))
  return len(aa)

count = 0
for g in groups: count += all_ans(g)

print("Part 2: " + str(count))
