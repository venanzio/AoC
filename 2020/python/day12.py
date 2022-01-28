print("Advent of Code 2020, Day 12")

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

x = 0
y = 0
wayx = 10
wayy = 1

def rotate(angle):
  global wayx
  global wayy
  angle = angle % 360
  if   angle==90:  wayx,wayy = -wayy,wayx
  elif angle==180: wayx,wayy = -wayx,-wayy
  elif angle==270: wayx,wayy = wayy,-wayx
  else: print("wrong angle")

def moveW(inst):
  global x
  global y
  global wayx
  global wayy
  if   inst[0]=='N': wayy += inst[1]
  elif inst[0]=="S": wayy -= inst[1]
  elif inst[0]=='E': wayx += inst[1]
  elif inst[0]=="W": wayx -= inst[1]
  elif inst[0]=="L": rotate(inst[1])
  elif inst[0]=="R": rotate(-inst[1])
  elif inst[0]=="F":
    x += inst[1] * wayx
    y += inst[1] * wayy
  else:
    print("invalid instruction")

for inst in instructions: moveW(inst)

print("Part 2: ", str(manhattan(x,y)))

  

