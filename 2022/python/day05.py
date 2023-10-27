# Advent of Code 2022, day 5
#   Venanzio Capretta

print("Advent of Code 2022, Day 5")

f = open("../input05")
input = f.readlines()
f.close()

# Parsing the input

def numberline(s):
  return (s[1]=='1')

def stack(s):
  st = ""
  for i in range(0,len(s),4):
    st += s[i+1]
  return st

sinput = []
while not numberline(input[0]):
  sinput.append(stack(input[0]))
  input.pop(0)

# eliminates numbers and blanks
input.pop(0)
input.pop(0)

print(sinput)
print(input[0])
 
# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

