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
  while s[i:]!='' and s[i].isdigit():
    sn += s[i]
    i += 1
  return((int(sn),s[i:].strip()))

def parse_list(s,parser):
  ls = s.split(',')
  l = []
  for xs in ls:
    (x,s) = parser(xs)
    l.append(x)
  return (l,s)

def parse_word(s,w):
  n = len(w)
  (w1,s) = (s[:n],s[n:])
  if w1==w:
    return (w,s)
  else:
    return None

def parse_monkey(s):
  (_,s) = parse_word(s,'Monkey')
  (n,s) = parse_num(s)
  print(n,s)
  return None


parse_monkey(input[0])

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

