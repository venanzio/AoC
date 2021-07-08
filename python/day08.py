# Advent of Code 2020, Day 7

with open("../input08", "r") as infile:
     lines = infile.readlines()

# Parse the program into a list of instructions

prog = []

for l in lines:
  (inst,val) = l.split(" ")
  prog.append((inst,int(val)))

# Part 1

# program length
size = len(prog)

# accumulator
acc = 0

# next instruction to be executed
inst = 0

# list of executed instructions
visited = []

def printstate():
  print("Accumulator: " + str(acc))
  print("Instruction: " + str(inst) + " -> " + str(prog[inst]))
  return ()

def execute((i,x)):
  global acc
  global inst
  if i == 'acc':
    acc += x
    inst +=1
  elif i == 'jmp':
    inst += x
  else:
    inst += 1
  return ()

while inst not in visited:
  visited.append(inst)
  print("visited list: " + str(visited))  
  execute(prog[inst])
  printstate()



print(acc)

