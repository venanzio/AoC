# Advent of Code 2022, day 2
#   Venanzio Capretta

print("Advent of Code 2022, Day 2")

f = open("../input02")
input = f.read().strip()
f.close()

# Parsing the input

# A,X = Rock; B,Y = Paper; C,Z = Scissors

strategy = input.split()

# Part 1

def convABC(c):
  return(ord(c)-ord('A'))

def convXYZ(c):
  return(ord(c)-ord('X'))

# "rotate" between Rock, Paper, and Scissors
def rotate(x,n):
  return(divmod(x+n,3)[1])

def value(mv):
  return (ord(mv)-ord('X')+1)

def score(v0,v1):
  match (divmod(v1-v0,3)[1]):
   case 0:
     return 3
   case 1:
     return 6
   case 2:
     return 0

def one_move(strategy):
  opp = convABC(strategy.pop(0))
  reply = convXYZ(strategy.pop(0))
  return(reply+1+score(opp,reply))

def part1(strategy):
  total = 0
  while strategy:
    total += one_move(strategy)
  return total

print("Part 1: ")
print(part1(strategy))

# Part 2

def score2(mv):
  match mv:
    case 'X':
      return 0
    case 'Y':
      return 3
    case 'Z':
      return 6

def two_move(strategy):
  opp = strategy.pop(0)
  res = strategy.pop(0)
  reply = ord(res)-ord('Y')
  return(value(reply)+score(opp,reply))

print("Part 2: ")
  

