# Advent of Code 2017 - day 3
println("Advent of Code 2017, day 3")

input = 289326

#Part 1

function square_lt(x) # least square smaller than x
  n = 0
  while (2n-1)^2 < x
    n += 1
  end
  return n-1
end

function frame_tour(x,n)
  x1 = x - (2n-1)^2
  if x1<=2n return(n,n-x1) end # right of frame
  x2 = x1-2n
  if x2<=2n return(n-x2,-n) end # top of frame
  x3 = x2-2n
  if x3<=2n return(-n,-n+x3) end # left of frame
  x4 = x3-2n
  return (-n+x4,n) # bottom of frame
end

function square_coords(x)
  if x == 1 return (0,0) end
  return (frame_tour(x,square_lt(x))) 
end

function manhattan(xy) return(abs(xy[1])+abs(xy[2])) end

println("part 1: ", manhattan(square_coords(input)))

# Part 2

directions = [(u,v) for u in [-1,0,1] for v in [-1,0,1] if (u,v) != (0,0)]

sum_grid = Dict((0,0) => 1)

function square_sum(i,j)
  s = 0
  for (u,v) in directions
    iu,jv = i+u,j+v
    if haskey(sum_grid,(iu,jv)) s += sum_grid[iu,jv] end
  end
  return s
end

square = 1
s = 1
while s <= input
  global square += 1
  (i,j) = square_coords(square)
  global s = square_sum(i,j)
  sum_grid[i,j] = s
end

println("part 2: ",s)