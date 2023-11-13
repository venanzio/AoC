# Advent of Code 2022, day 11
#   Venanzio Capretta

print("Advent of Code 2022, Day 11")

f = open("../input11")
input = f.read().strip()
f.close()

# Parsing the input

class Monkey:
  def __init__(self,items,operation,test,throwif,throwelse):
    self.items = items
    self.operation = operation
    self.test = test
    self.throwif = throwif
    self.throwelse = throwelse
    self.activity = 0

m = Monkey([1,2],lambda x: x*11,lambda x: x//5==0,7,4)

print(m.items)

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

