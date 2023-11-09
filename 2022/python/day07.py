# Advent of Code 2022, day 
#   Venanzio Capretta

print("Advent of Code 2022, Day 7")

f = open("../input07")
input = f.read().splitlines()
f.close()

# data structure

class filesystem:
  def __init__(self,name):
    self.name = name
    self.parent = None
    self.is_file = False
    self.size = -1  # -1 means it hasn't been computed yet
    self.children = []

  def __str__(self):
    return f"{self.name} ({self.size})"+str([str(x) for x in self.children])

  def get_size(self):
    if self.size == -1:
      self.size = sum([x.get_size() for x in self.children])
    return self.size

class directory(filesystem):
  def add_child(self,child):
    child.parent = self
    self.children.append(child)

  def find_dir(self,nm):
    for c in self.children:
      if c.name == nm:
        return c
    # if not found, create it
    c = directory(nm)
    self.add_child(c)
    return c

  def find_file(self,nm,sz):
    for c in self.children:
      if c.name == nm:
        return c
    # if not found, create it
    c = file(nm,sz)
    self.add_child(c)
    return c

class file(filesystem):
  def __init__(self,name,size):
    self.name = name
    self.parent = None
    self.is_file = True
    self.size = size
    self.children = []

  def __str__(self):
    return f"{self.name} ({self.size})"

# Parsing the input

fs = directory("/")
x = fs

for l in input:
  if l == "$ cd /":
    x = fs
  elif l == "$ cd ..":
    x = x.parent
  elif l[:5] == "$ cd ":
    x = x.find_dir(l[5:])
  elif l[0].isdigit():
    sznm = l.split()
    x.find_file(sznm[1],int(sznm[0]))
  # no need to parse "$ ls" and it's output

print(fs)

# Part 1
def sizes(fs):
  ch_sz = sum([sizes(x) for x in fs.children if x.is_file==False],[])
  if fs.get_size() <= 100000:
    ch_sz.append(fs.size)
  return ch_sz
    
print("Part 1: ")
print(sum(sizes(fs)))

# Part 2

print("Part 2: ")
  

