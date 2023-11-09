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

  def get_size(self):
    if self.size == -1:
      self.size = sum([x.get_size() for x in self.children])
    return self.size

class node(filesystem):
  def add_child(self,child):
    self.children.append(child)

class leaf(filesystem):
  def __init__(self,name,size):
    self.name = name
    self.size = size
    self.children = []

  def __str__(self):
    return f"{self.name} ({self.size})"

fs1 = node("/")
fs2 = node("d1")
fs1.add_child(leaf("a",27))
fs1.add_child(fs2)
fs2.add_child(leaf("b",83))
fs2.add_child(leaf("c",31))

print(fs1)

print(fs1.get_size())

print(fs1)

# ...

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

