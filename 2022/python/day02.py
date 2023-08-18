# Advent of Code 2022, day ?
#   Venanzio Capretta

print("Advent of Code 2022, Day ?")

f = open("../input02")
input = f.read().strip()
f.close()

# Parsing the input

# ...

strategy = input.split()

# Part 1

def value(mv):
  match mv:
    case 'X':
      return 1
    case 'Y':
      return 2
    case 'Z':
      return 3

def score(m0,m1):
  return 0

def one_move():
  opp = strategy[0]
  reply = strategy[1]
  strategy = strategy[2:]
  return(value(reply)+score(opp,reply))


print("Part 1: ")


# Part 2

print("Part 2: ")
  

