# Advent of Code 2022, day 11
#   Venanzio Capretta

import parsers as P

print("Advent of Code 2022, Day 11")

f = open("../input11")
input = f.read()
f.close()

source = P.Source(input)

# Parsing the input

class Monkey:
  def __init__(self,items,operation,testnum,throwT,throwF):
    self.items = items
    self.operation = operation
    self.testnum = testnum
    self.throwT = throwT
    self.throwF = throwF
    self.activity = 0

def parse_arg(s):
  P.space(s)
  if P.word('old',s):
    return (lambda x:x)
  else:
    n = P.num(s)
    return (lambda x: n)

def parse_op(s):
  P.word('Operation: new =',s)
  f1 = parse_arg(s)
  P.space(s)  
  ops = P.char(s)
  f2 = parse_arg(s)
  if ops == '*':
    return (lambda x: f1(x) * f2(x))
  elif ops == '+':
    return (lambda x: f1(x) + f2(x))
  else:
    return None

def parse_monkey(s):
  # monkey number (not needed)
  P.word('Monkey',s)
  n = P.num(s)
  P.newline(s)

  # list of items
  P.word('Starting items:',s)
  its = P.list(s,P.num)
  P.newline(s)

  # operation on items
  op = parse_op(s)

  return op

print(parse_monkey(source)(7))






'''
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

def parse_arg(s):
  s = s.strip()
  if s[:3] == 'old':
    return (lambda x:x, s[3:].strip())
  else:
    (n,s) = parse_num(s)
    return (lambda x: n, s)

def parse_op(s):
  s = s.strip()
  (_,s) = parse_word(s,'Operation: new =')
  (f1,s) = parse_arg(s)
  ops = s[0]
  (f2,s) = parse_arg(s[1:])
  if ops == '*':
    return (lambda x: f1(x) * f2(x), s)
  elif ops == '+':
    return (lambda x: f1(x) + f2(x), s)
  else:
    return None

def parse_test(s):
  s = s.strip()
  (_,s) = parse_word(s,'Test: divisible by')
  (n,s) = parse_num(s)
  return (n, s)

def parse_monkey(ss):
  s = ss[0]
  (_,s) = parse_word(s,'Monkey')
  (n,s) = parse_num(s)
  s = ss[1]
  (_,s) = parse_word(s,'Starting items:')
  (its,_) = parse_list(s,parse_num)
  s = ss[2]
  (op,s) = parse_op(s)
  s = ss[3]
  (test,s) = parse_test(s)
  s = ss[4]
  (_,s) = parse_word(s,'If true: throw to monkey')
  (throwT,s) = parse_num(s)
  s = ss[5]
  (_,s) = parse_word(s,'If false: throw to monkey')
  (throwF,s) = parse_num(s)
  return Monkey(its,op,test,throwT,throwF)

monkeys = [parse_monkey(input[i:i+6]) for i in range(0,len(input),7)]

# Part 1

all_items = [m.items.copy() for m in monkeys] # to restore them in part 2

for i in range(1,21):
  for m in monkeys:
    m.activity += len(m.items)
    for it in m.items:
      it = m.operation(it) // 3
      if it % m.testnum == 0:
        monkeys[m.throwT].items.append(it)
      else:
        monkeys[m.throwF].items.append(it)
    m.items = []

mas = [m.activity for m in monkeys]
mas.sort(reverse=True)

print('Part 1: ')
print(mas[0] * mas[1])

# Part 2

import numpy

for i in range(len(monkeys)):
  m = monkeys[i]
  m.items = all_items[i]
  m.activity = 0

max_worry = numpy.prod([m.testnum for m in monkeys])

for i in range(1,10001):
  for m in monkeys:
    m.activity += len(m.items)
    for it in m.items:
      it = m.operation(it) % max_worry
      if it % m.testnum == 0:
        monkeys[m.throwT].items.append(it)
      else:
        monkeys[m.throwF].items.append(it)
    m.items = []


mas = [m.activity for m in monkeys]
mas.sort(reverse=True)

print('Part 2: ')
print(mas[0] * mas[1])

'''
