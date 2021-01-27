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

# Part 1
valid = 0
for (mn,mx,c,w) in plist:
  if mn <= count(c,w) <= mx: valid+=1
print('Part 1: ' + str(valid))











valid_2 = []
valid_1 = []
for lin in lines:
    key, value = lin.split(": ")
    char = key.split(" ")[1]
    # print(char, key.split(" ")[0].split("-"))
    n1, n2 = key.split(" ")[0].split("-")
    n1, n2 = int(n1)-1, int(n2)-1

    if n1+1<=value.count(char)<=n2+1:
        valid_1.append(lin)

    c = 0
    try:

        if value[int(n1)] == char:
            c+=1
        if value[int(n2)] == char:
            c+=1
        if c == 1:
            valid_2.append(lin)
    except:
        pass
print("Solution 1: " + str(len(valid_1)))
print("Solution 2: " + str(len(valid_2)))

