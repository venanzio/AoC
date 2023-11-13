# Advent of Code 2022, day 11
#   Venanzio Capretta

print("Advent of Code 2022, Day 11")

f = open("../input11")
input = f.read().splitlines()
f.close()

# Parsing the input

class Monkey:
  def __init__(self,items,operation,test,throwT,throwF):
    self.items = items
    self.operation = operation
    self.test = test
    self.throwT = throwT
    self.throwF = throwF
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

def parse_test(s):
  s = s.strip()
  (_,s) = parse_word(s,'Test: divisible by')
  (n,s) = parse_num(s)
  return (lambda x: x // n == 0,s)

def parse_monkey(ss):
  s = ss[0]
  (_,s) = parse_word(s,'Monkey')
  (n,s) = parse_num(s)
  s = ss[1]
  (_,s) = parse_word(s,'Starting items:')
  (its,_) = parse_list(s,parse_num)
  s = ss[2]
  op = parse_op(s)
  s = ss[3]
  test = parse_test(s)
  s = ss[4]
  (_,s) = parse_word(s,'If true: throw to monkey')
  (throwT,s) = parse_num(s)
  s = ss[5]
  (_,s) = parse_word(s,'If false: throw to monkey')
  (throwF,s) = parse_num(s)
  return Monkey(its,op,test,throwT,throwF)



monkeys = []

#monkeys.append(parse_monkey(input[0:6]))
#rint(monkeys[0].items)


r = range(0,len(input),7)
for i in r[:4]:
  monkeys.append(parse_monkey(input[i:i+6]))

for m in monkeys:
  print(m.items)

# Part 1

print("Part 1: ")


# Part 2

print("Part 2: ")
  

