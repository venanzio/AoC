# Advent of Code 2020, Day 7

with open("../input08", "r") as infile:
     lines = infile.readlines()

# Parses the program into a list of instructions
prog = []

for l in lines:
  (inst,val) = l.split(" ")
  prog.append((inst,int(val)))

print(prog)
