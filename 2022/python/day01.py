# Advent of Code 2022, day 1
#   Venanzio Capretta

print("Advent of Code 2022, Day 1")

f = open("../input")
input = f.read().strip()
f.close()

# Parsing the input

# split a string in blocks separated by empty lines
def blocks(s):
  return (s.split('\n\n'))

# list of calories for one elf
def elfcal(s):
  return [int(x) for x in s.split()]

# calories for each food item for each elf
foodcal = [elfcal(s) for s in blocks(input)]

# Part 1

print("Part 1: ")

calories = [sum(xs) for xs in foodcal]

print(max(calories))

# Part 2

print("Part 2: ")
  
calories.sort(reverse=True)

print(sum(calories[0:3]))
