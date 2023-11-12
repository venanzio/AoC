# Advent of Code 2022, day 10
#   Venanzio Capretta

print("Advent of Code 2022, Day 10")

f = open("../input10")
input = f.read().splitlines()
f.close()

# Parsing the input

program = []
for s in input:
  if s[0:4] == 'noop':
    program.append(('noop',0))
  else:
    program.append(('addx',int(s[4:])))

# Part 1

actions = [0]
for (inst,x) in program:
  actions.append(0)
  if inst == 'addx':
    actions.append(x)

def signal_strength(c,x):
  return c*x

registerX = 1
sig_sum = 0
for cycle in range(len(actions)):
  if (cycle-20)%40 == 0:
    sig_sum += signal_strength(cycle,registerX)
  registerX += actions[cycle]

print("Part 1: ")
print(sig_sum)

# Part 2

registerX = 1
crt = []
for i in range(6):
  crt_row = []
  for j in range(40):
    cycle = 40*i+j+1
    if abs(j-registerX) <= 1:
      crt_row.append('#')
    else:
      crt_row.append('.')
    registerX += actions[cycle]
  crt.append(crt_row)

print("Part 2: ")
for i in range(6):
  print(''.join(crt[i]))

