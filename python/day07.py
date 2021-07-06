# Advent of Code 2020, Day 7

with open("../input07", "r") as infile:
     lines = infile.readlines()

# Parses a number (quantity of bags) and bag name
def parse_num_bag(s):
  (n,bs) = s.split(" ",1)
  bag = bs.split(" bag")[0]
  return (int(n), bag)

# Parse a bag content specification
def bag_parse(line):
  (bag,spec) = line.split(" bags contain ")
  if spec.startswith("no"): return(bag,[])
  nbs = spec.split(", ")
  conts = [parse_num_bag(nb) for nb in nbs]
  return(bag,conts)

# a dictionary mapping bag names to their contents
bags = {}
for line in lines:
  (bag,conts) = bag_parse(line)
  bags.update({bag : conts})

# Part 1

# dictionary of contents (as sets without quantity)

bcont = { }
for bag in bags:
  conts = set([b for (n,b) in bags[bag]])
  bcont.update({bag : conts})

# construct the set of bags recursively containing a shiny gold bag
sbags = set()
newbs = { "shiny gold" }  # bags added on previous pass
more = True   # have we added some bags in the previous pass?
while more: 
  more = False
  nextbs = set()  # bags added on this pass
  for bag in bcont.keys():
    cont = bcont[bag]
    if not(newbs.isdisjoint(cont)): 
      nextbs.add(bag)
      bcont.pop(bag)
      more = True
  sbags.update(nextbs)
  newbs = nextbs

print("Part 1: " + str(len(sbags)))

# Part 2

# Recursively count how many bags are contained in bag
def rec_cont(bag):
  return(sum([n * (1 + rec_cont(b))  for (n,b) in bags[bag]]))

print("Part 2: " + str(rec_cont("shiny gold")))
