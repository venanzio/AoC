# Advent of Code 2022, day 10
#   Venanzio Capretta

print("Advent of Code 2022, Day 10")

f = open("../input10")
input = f.read().splitlines()
f.close()

# Parsing the input

program = []
for s in input:
  if s[0:4] == 'noop':
    program.append(('noop',0))
  else:
    program.append(('addx',int(s[4:])))

# Part 1

cycle = 1
registerX = 1


def signal_strength():
  return cycle*registerX

print(signal_strength())

print("Part 1: ")


# Part 2

print("Part 2: ")
  

