# Advent of Code 2020, Day 7

with open("../input08", "r") as infile:
     lines = infile.readlines()

# Parse the program into a list of instructions

prog = []

for l in lines:
  (inst,val) = l.split(" ")
  prog.append((inst,int(val)))

# program length
size = len(prog)

# accumulator
acc = 0

# next instruction to be executed
inst = 0





print(size)

