# Advent of Code 2020, Day 2

with open("../input02", "r") as infile:
     input = infile.readlines()

# divide the input into lines
lines = [i.split("\n")[0] for i in input]

# parse a single line, return (min,max,ch,word)
def parse_line(line):
  spl = line.split(' ')
  print(spl)
  ns = spl[0].split('-')
  min = int(ns[0])
  max = int(ns[1])
  ch = spl[1].split(':')[0]
  word = spl[2]
  return (min,max,ch,word)

# list of password specifications
plist = [parse_line(l) for l in lines]

# count how many times an item is in a sequence (char in str)
def count(it,seq):
  c = 0
  for x in seq:
   if x==it: c+=1
  return c

# Solutions

valid1 = 0
valid2 = 0

for (mn,mx,c,w) in plist:
  if mn <= count(c,w) <= mx: valid1+=1
  if (w[mn-1]==c) != (w[mx-1]==c): valid2+=1

print('Part 1: ' + str(valid1))
print('Part 2: ' + str(valid2))
