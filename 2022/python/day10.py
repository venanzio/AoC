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


#action_range = range(0,len(program)+3)

#actions = [0 for i in action_range]
#for i in range(len(program)):
#  if program[i][0] == 'addx':
#    actions[i+1] += program[i][1]

actions = [0]
for i in range(len(program)):
  actions.append(0)
  if program[i][0] == 'addx':
    actions.append(program[i][1])

print(actions)

def signal_strength(c,x):
  return c*x

registerX = 1
sig_sum = 0
for cycle in range(len(actions)):
  if (cycle-20)%40 == 0:
    sig_sum += signal_strength(cycle,registerX)
    print('cycle ',cycle,' - sum = ',sig_sum)
  registerX += actions[cycle]

print("Part 1: ")
print(sig_sum)

# Part 2

print("Part 2: ")
  

