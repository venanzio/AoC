# Advent of Code 2017 - day 2

f = open("input2_r")
input = readlines(f)
close(f)

function parse_nums(s)
  [parse(Int,n) for n in split(s)]
end

spreadsheet = [parse_nums(s) for s in input]

function diff(l)
  maximum(l)-minimum(l)
end

checksum = sum([diff(l) for l in spreadsheet])

println("part 1: ", checksum)

function divides(x,y)
  y !=x && rem(y,x)==0
end

function even_div(l)
  for x in l
    for y in l
      if divides(x,y) return div(y,x) end
    end
  end
end

div_sum = sum([even_div(l) for l in spreadsheet])

println("part 2: ", div_sum)

