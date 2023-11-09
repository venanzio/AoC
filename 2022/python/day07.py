# Advent of Code 2022, day 
#   Venanzio Capretta

print("Advent of Code 2022, Day 7")

f = open("../input07")
input = f.read().strip()
f.close()

# Parsing the input

class filesystem:
  def __init__(self,name):
    self.name = name
    self.size = -1
    self.children = []

  def __str__(self):
    return f"{self.name} ({self.size})"+str([str(x) for x in self.children])

class node(filesystem):
  def add_child(self,child):
    self.children.append(child)

  def get_size(self):
    self.size = sum([x.size for x in self.children])
    return self.size

class leaf(filesystem):
  def __init__(self,name,size):
    self.name = name
    self.size = size
    self.children = []

  def __str__(self):
    return f"{self.name} ({self.size})"

fs1 = node("/")
fs2 = node("d1")
l1 = leaf("a",27)
l2 = leaf("b",83)

fs1.add_child(l1)
fs1.add_child(fs2)
fs2.add_child(l2)

print(fs1)

fs1.get_size

print(fs1)

# ...

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

