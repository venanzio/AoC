# Advent of Code 2020, Day 12

f = open("../input")
input = f.readlines() 
f.close()

# Parsing the input

def parse_inst(s):
  return (s[0],int(s[1:]))

instructions = list(map(parse_inst,input))

# Part 1

# (x,y,d) coordinates and direction of the ship
x = 0
y = 0
d = 0

def move(inst):
  global x
  global y
  global d
  if   inst[0]=='N': y += inst[1]
  elif inst[0]=="S": y -= inst[1]
  elif inst[0]=='E': x += inst[1]
  elif inst[0]=="W": x -= inst[1]
  elif inst[0]=="L": d = (d+inst[1]) % 360
  elif inst[0]=="R": d = (d-inst[1]) %360
  elif inst[0]=="F":
    if   d==0: x += inst[1]
    elif d==90: y += inst[1]
    elif d==180: x -= inst[1]
    elif d==270: y -= inst[1]
    else: printf("invalid direction")
  else:
    print("invalid instruction")

def manhattan(x,y):
  return abs(x)+abs(y)

for inst in instructions: move(inst)

print("Part 1: ", str(manhattan(x,y)))

# Part 2

wayx = 10
wayy = 0

print("Part 2: ")
  

