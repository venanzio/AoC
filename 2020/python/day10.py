# Advent of Code 2020, Day 10

# Parsing the input

input = open("../input")

adapters = [0]
for l in input.readlines():
  adapters.append(int(l))

input.close()


# Part 1

adapters.sort()
adapters.append(adapters[-1]+3)

ones = 0
threes = 0

for i in range(1,len(adapters)):
  diff = adapters[i]-adapters[i-1]
  if diff==1: ones+=1
  elif diff==3: threes+=1

print("Part 1: " + str(ones*threes))


# Part 2

paths = {0:1}
adapters.remove(0)

for a in adapters:
  paths[a] = 0
  for i in range(1,4):
    x = a-i
    if x in paths: paths[a] += paths[x]

print("Part 2: " + str(paths[adapters[-1]]))
  

