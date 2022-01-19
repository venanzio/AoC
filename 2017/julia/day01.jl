# Advent of Code 2017 - day 1

f = open("../input")
input = read(f,String)
close(f)

lastIndx = lastindex(input)-1 # ignore ending newline

s = 0

for i = firstindex(input):lastIndx
  j = i==lastIndx ? 1 : i+1
  if input[i] == input[j]
    global s += parse(Int,input[i])
  end
end

println("part 1: ",s)