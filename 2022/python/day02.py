# Advent of Code 2022, day ?
#   Venanzio Capretta

print("Advent of Code 2022, Day ?")

f = open("../input02")
input = f.read().strip()
f.close()

# Parsing the input

# A,X = Rock; B,Y = Paper; C,Z = Scissors

strategy = input.split()

# Part 1

def value(mv):
  return (ord(mv)-ord('X')+1)

def score(m0,m1):
  v0 = ord(m0) - ord('A')
  v1 = ord(m1) - ord('X')
  match (divmod(v1-v0,3)[1]):
   case 0:
     return 3
   case 1:
     return 6
   case 2:
     return 0

def one_move():
  global strategy
  opp = strategy.pop(0)
  reply = strategy.pop(0)
  return(value(reply)+score(opp,reply))

total = 0
while strategy:
  total += one_move()

print("Part 1: ")
print(total)

# Part 2

print("Part 2: ")
  

