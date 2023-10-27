# Advent of Code 2022, day 5
#   Venanzio Capretta

print("Advent of Code 2022, Day 5")

f = open("../input05")
input = f.readlines()
f.close()

# Parsing the input

# stacks 

def numberline(s):
  return (s[1]=='1')

def stack(s):
  st = ""
  for i in range(0,len(s),4):
    st += s[i+1]
  return st

sinput = []
while not numberline(input[0]):
  sinput.append(stack(input[0]))
  input.pop(0)

def transpose(l):
  tr = []
  for i in range(0,9):
    tr.append("")
    for j in range(0,len(l)):
      tr[i] += l[j][i]
  return tr

stacks = transpose(sinput)
for i in range(0,len(stacks)):
  stacks[i] = stacks[i].strip()

print(stacks)

# moves

# eliminates numbers and blanks
input.pop(0)
input.pop(0)

def move(s):
  (x,s1) = s.split("move ",1)
  (num,s2) = s1.split(" from ",1)
  (frs,tos) = s2.split(" to ",1)
  return (int(num),int(frs),int(tos))

moves = []
for m in input:
  moves.append(move(m))

print(moves)
 
# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

