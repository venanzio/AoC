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

stacks = [[] for i in range(9)]
while not numberline(input[0]):
  i = 0
  for k in range(0,len(input[0]),4):
    if input[0][k+1] != ' ' :
      stacks[i].append(input[0][k+1])
    i += 1
  input.pop(0)

for s in stacks:
  s.reverse()

# print(stacks)

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

# print(moves)
 
# Part 1

for (n,s,t) in moves:
  for i in range(n):
    stacks[t-1].append(stacks[s-1].pop())

print("Part 1: ")

answer = ""
for l in stacks:
  answer += l[-1]

print(answer)

# Part 2

print("Part 2: ")
  

