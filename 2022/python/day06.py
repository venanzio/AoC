# Advent of Code 2022, day 6
#   Venanzio Capretta

print("Advent of Code 2022, Day 6")

f = open("../input06")
input = f.read().strip()
f.close()

# Part 1

def marker(s,n):
  return (len(set(s)) == n)

def find_marker(ins,n):
  i = n
  while not marker(input[i-n:i],n):
    i += 1
  return i

print("Part 1: ")
print(find_marker(input,4))

# Part 2

print("Part 2: ")
print(find_marker(input,14))  

