# Advent of Code 2020, Day 8

with open("../input", "r") as infile:
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

# executes an instruction
def execute(ix,acc,inst):
  (i,x) = ix
  if i == 'acc':
    acc += x
    inst +=1
  elif i == 'jmp':
    inst += x
  else:
    inst += 1
  return (acc,inst)

# check if a program terminates, returns final accumulator
def terminate(prog):
  size = len(prog)
  acc = 0
  inst = 0
  visited = []
  while (inst < size) and (inst not in visited):
    visited.append(inst)
    (acc,inst) = execute(prog[inst],acc,inst)
  return (inst >= size, acc, visited)

(t,acc,visited) = terminate(prog)

print("Part 1: " + str(acc))

# Part 2

def chinstr(ix):
  (i,x) = ix
  if i == "jmp":
    return (("nop",x))
  elif i == "nop":
    return (("jmp",x))
  else:
    return ((i,x))

for i in visited:
  prog[i] = chinstr(prog[i])
  (t,a,v) = terminate(prog)
  if t: 
    print("Part 2: " + str(a))
    break
  prog[i] = chinstr(prog[i]) # changing back
  

