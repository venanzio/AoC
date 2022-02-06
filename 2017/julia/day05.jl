println("Advent of Code 2017, day 5")

f = open("input5")
input = readlines(f)
close(f)

jumps = [parse(Int,x) for x in input]

# Part 1

l = length(jumps)
steps = 0
i = 1
while 1 <= i <= l
  j = jumps[i]
  jumps[i] = j+1
  global i += j
  global steps += 1
end

println("Part 1: ", steps)

# Part 2

jumps = [parse(Int,x) for x in input]

steps = 0
i = 1
while 1 <= i <= l
  j = jumps[i]
  if j >= 3
    jumps[i] = j-1
  else 
    jumps[i] = j+1
  end
  global i += j
  global steps += 1
end

println("Part 2: ", steps)