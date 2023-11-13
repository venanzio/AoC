# Advent of Code 2022, day 11
#   Venanzio Capretta

print("Advent of Code 2022, Day 11")

f = open("../input11")
input = f.read().splitlines()
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

def parse_num(s):
  s = s.strip()
  sn = ''
  i = 0
  while s[i].isdigit():
    sn += s[i]
    i += 1
  return((int(sn),s[i:].strip()))




def parse_monkey(s):
  return None  

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

