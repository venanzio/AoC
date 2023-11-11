# Advent of Code 2022, day 9
#   Venanzio Capretta

print("Advent of Code 2022, Day 9")

f = open("../input09")
input = f.read().splitlines()
f.close()

# Parsing the input

def move(s):
  (mv,steps) = s.split(' ',1)
  return (mv,int(steps))

moves = [move(s) for s in input]

print(moves)

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

