# Advent of Code 2022, day 
#   Venanzio Capretta

print("Advent of Code 2022, Day 7")

f = open("../input07")
input = f.read().strip()
f.close()

# Parsing the input

class filesystem:
  def __init__(self,name,size):
    self.name = name
    self.size = size
    self.children = []

  def __str__(self):
    return f"{self.name} ({self.size}) {self.children}"


fs = filesystem("/",-1)

print(fs)

# ...

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

