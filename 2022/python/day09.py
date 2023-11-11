# Advent of Code 2022, day 9
#   Venanzio Capretta

print("Advent of Code 2022, Day 9")

f = open("../input09")
input = f.read().splitlines()
f.close()

# Parsing the input

def parse_move(s):
  (mv,steps) = s.split(' ',1)
  return (mv,int(steps))

moves = [parse_move(s) for s in input]

# Part 1

def distance(x1,y1,x2,y2):
  return max(abs(x1-x2),abs(y1-y2))

def sign(x):
  if x==0:
    return 0
  else:
    return x//abs(x)

def move(x,y,dir):
  if dir == 'U':
    return (x,y+1)
  elif dir == "D":
    return (x,y-1)
  elif dir == "R":
    return (x+1,y)
  elif dir == "L":
    return (x-1,y)
  else:
    return None

def follow(hx,hy,tx,ty):
  if distance(hx,hy,tx,ty) <= 1:
    return (tx,ty)
  else:
    return (tx+sign(hx-tx), ty+sign(hy-ty))

visited = {(0,0)}

(hx,hy) = (tx,ty) = (0,0)

for (dir,steps) in moves:
  for i in range(steps):
    (hx,hy) = move(hx,hy,dir)
    (tx,ty) = follow(hx,hy,tx,ty)
    visited.add((tx,ty))
  

print("Part 1: ")
print(len(visited))



# Part 2

print("Part 2: ")
  

