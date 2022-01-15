# Advent of Code 2020, Day 10

# Parsing the input

input = open("../input")

adapters = []
for l in input.readlines():
  adapters.append(int(l))

input.close()

# Part 1

adapters.sort()
adapters.append(adapters[-1]+3)

print(adapters)

print("Part 1: ")


# Part 2

print("Part 2: ")
  

