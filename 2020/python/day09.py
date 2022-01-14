# Advent of Code 2020, Day 9

with open("../input", "r") as infile:
     lines = infile.readlines()

# Parsing the input

nums = list(map(int,lines))

# Part 1

pre = nums[0:25]
rest = nums[25:-1]

def pairSum(y):
  for x0 in pre:
    for x1 in pre:
      if (not x0 == x1) and (y == x0+x1): return True
  return False

while rest:
  y = rest.pop(0)
  if pairSum(y):
    pre.pop(0)
    pre.append(y)
  else: break

print("Part 1: " + str(y))

# Part 2

run = []
sum = 0

while not sum==y:
  # remove initial elements to bring sum below y
  while sum>y:
    x = run.pop(0)
    sum -= x
  # add new element until you reach y
  while sum<y:
    x = nums.pop(0)
    run.append(x)
    sum += x

run.sort()

print("Part 2: " + str(run[0] + run[-1]))
  

