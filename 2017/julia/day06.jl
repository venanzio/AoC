println("Advent of Code 2017, day 6")

f = open("input6")
input = read(f,String)
close(f)

banks = [parse(Int,n) for n in split(input)]
n = length(banks)

# Part 1

function next(j)
  if j<n return j+1 else return 1 end
end

function redistribute(bk)
  b = copy(bk)
  (m,j) = findmax(b)
  b[j] = 0
  while m > 0 
    j = next(j)
    b[j] += 1
    m -= 1
  end
  return b
end

cycles = [ banks ]
b = redistribute(banks)
i = 1
while !(b in cycles)
  append!(cycles,[b])
  global b = redistribute(b)
  global i += 1
end

println("Part 1: ",i)

# Part 2

index = findfirst(x -> x==b,cycles)

println("Part 2: ",i-index+1)
