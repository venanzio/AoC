# Advent of Code 2020, Day 7

with open("../input07", "r") as infile:
     lines = infile.readlines()

# Parses a number (quantity of bags) and bag adjective and name
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

bags = {}
for line in lines:
  (bag,conts) = bag_parse(line)
  bags.update({bag : conts})


