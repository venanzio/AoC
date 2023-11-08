# Advent of Code 2022, day 6
#   Venanzio Capretta

print("Advent of Code 2022, Day 6")

f = open("../input06")
input = f.read().strip()
f.close()

# Part 1

def marker(s):
  return (len(set(s)) == 4)

i = 4
while not marker(input[i-4:i]):
  i += 1

print("Part 1: ")
print(i)

# Part 2

print("Part 2: ")
  

