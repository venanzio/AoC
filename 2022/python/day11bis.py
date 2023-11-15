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
  P.word(':',s)

  # list of items
  P.word('Starting items:',s)
  its = P.list(P.num,s)

  # operation on items
  op = parse_op(s)

  # test on items
  P.word('Test: divisible by',s)
  test = P.num(s)
  P.word('If true: throw to monkey',s)
  throwT = P.num(s)
  P.word('If false: throw to monkey',s)
  throwF = P.num(s)

  return Monkey(its,op,test,throwT,throwF)

monkeys = []
while not P.space(source):
  monkeys.append(parse_monkey(source))


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


