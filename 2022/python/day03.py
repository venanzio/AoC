# Advent of Code 2022, day 3
#   Venanzio Capretta

print("Advent of Code 2022, Day 3")

f = open("../input03")
input = f.read().strip()
f.close()

# Parsing the input

rucksacks = input.splitlines()

# Part 1

def common(s1,s2):
  return ''.join(set(s1).intersection(s2))

def wrong_item(r):
  n = len(r) // 2
  r1 = r[:n]
  r2 = r[n:]
  return common(r1,r2)

def priority(c):
  if c.islower():
    return (ord(c)-ord('a')+1)
  else:
    return (ord(c)-ord('A')+27)

print("Part 1: ")
print(sum([priority(wrong_item(r)) for r in rucksacks]))

# Part 2

def take(l,n):
  tl = []
  for i in range(n):
    tl.append(l.pop(0))
  return tl

s = 0
while rucksacks:
  group = take(rucksacks,3)
  badge = set(group[1]).intersect(group[2],group[3])
  s += priority(badge)

print("Part 2: ")
print(s)
