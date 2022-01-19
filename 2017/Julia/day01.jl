# Advent of Code 2017 - day 1

f = open("../input")
input = read(f,String)
close(f)

s = 0

for i = firstindex(input):lastindex(input)
  j = i==lastindex(input) ? 1 : i+1
  if input[i] == input[j]
    global s += parse(Int,input[i])
  end
end

println(s)