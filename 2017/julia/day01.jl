# Advent of Code 2017 - day 1

f = open("input1_r")
input = read(f,String)
close(f)

input = filter(isdigit,input)

lastIndx = lastindex(input)
s = 0
for i = firstindex(input):lastIndx
  j = i==lastIndx ? 1 : i+1
  if input[i] == input[j]
    global s += parse(Int,input[i])
  end
end

println("part 1: ",s)

halfIndx = div(lastIndx,2)
s = 0
for i = firstindex(input):halfIndx
  j = i+halfIndx
  if input[i] == input[j]
    global s += 2*parse(Int,input[i])
  end
end

println("part 2: ",s)