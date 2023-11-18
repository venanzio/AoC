# Advent of Code 2022, day 13
#   Venanzio Capretta

import parsers as P

print("Advent of Code 2022, Day 13")

f = open("../input13")
input = [P.Source(l) for l in f.read().splitlines()]
f.close()

# Parsing the input

def parse_packet(s):
  x = P.num(s)
  if x != None:
    return x
  elif s.next() == '[':
    P.word('[',s)
    l = P.lst(parse_packet,s)
    P.word(']',s)
    return l
  else:
    return None

packets = [parse_packet(l) for l in input if l.text!='']

# Part 1

def lexicographic(ord,l1,l2):
  if l1 == []:
    return True
  elif l2 == []:
    return False
  elif l1[0] == l2[0]:
    return lexicographic(ord,l1[1:],l2[1:])
  else:
    return ord(l1[0],l2[0])

def packet_ord(p1,p2):
  if type(p1)==type(p2):
    if type(p1) == type(0):
      return p1<p2
    else:
      return lexicographic(packet_ord,p1,p2)
  else:
    if type(p1) == type(0):
      return packet_ord([p1],p2)
    else:
      return packet_ord(p1,[p2])


print(packet_ord([1,1,3,1,1],[1,1,5,1,1]))


s=0
for i in range(0,len(packets),2):
  if packet_ord(packets[i],packets[i+1]):
    s += i+1

print("Part 1: ")
print(s)

# Part 2

print("Part 2: ")
  

