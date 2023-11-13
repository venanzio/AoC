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
  s = s.strip()
  n = len(w)
  (w1,s) = (s[:n],s[n:])
  if w1==w:
    return (w,s.strip())
  else:
    return None

def parse_op(s):
  s = s.strip()
  (_,s) = parse_word(s,'Operation: new = old')
  ops = s[0]
  (n,s) = parse_num(s[1:])
  if ops == '*':
    return (lambda x: x * n,s)
  elif ops == '+':
    return (lambda x: x + n,s)
  else:
    return None

def parse_monkey(ss):
  s = ss[0]
  (_,s) = parse_word(s,'Monkey')
  (n,s) = parse_num(s)
  s = ss[1]
  (_,s) = parse_word(s,'Starting items:')
  its = parse_list(s,parse_num)
  s = ss[2]
  op = parse_op(s)
  return None

print(input[2])
(op,_) = parse_op(input[2])
print(op(8))



# parse_monkey(input[7:])

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

