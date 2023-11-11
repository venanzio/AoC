# Advent of Code 2022, day 9
#   Venanzio Capretta

print("Advent of Code 2022, Day 9")

f = open("../input09")
input = f.read().splitlines()
f.close()

# Parsing the input

def move(s):
  (mv,steps) = s.split(' ',1)
  return (mv,int(steps))

moves = [move(s) for s in input]

# Part 1

def distance(x1,y1,x2,y2):
  return max(abs(x1-x2),abs(y1-y2))

def sign(x):
  if x==0:
    return 0
  else:
    return x//abs(x)

def follow(hx,hy,tx,ty):
  if distance(hx,hy,tx,ty) <= 1:
    return (tx,ty)
  else:
    return (tx+sign(hx-tx), ty+sign(hy-ty))

print(follow(4,7,2,8))

print("Part 1: ")


# Part 2

print("Part 2: ")
  

