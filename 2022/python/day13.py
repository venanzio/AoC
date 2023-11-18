# Advent of Code 2022, day 13
#   Venanzio Capretta

import parsers as P

print("Advent of Code 2022, Day 13")

f = open("../input13")
input = [P.Source(l) for l in f.read().splitlines()]
f.close()

# Parsing the input

# Part 1

def parse_packet(s):
  x = P.num(s)
  if x != None:
    return x
  elif s.next() == '[':
    P.word('[',s),
    l = P.lst(parse_packet,s),
    P.word(']',s)
    return l[0]  # I don't understand why I must do this
  else:
    return None

print(parse_packet(P.Source('[0,3,7]')))

print("Part 1: ")


# Part 2

print("Part 2: ")
  

