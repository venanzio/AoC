# Advent of Code 2020, Day 3

import re

with open("../input04", "r") as infile:
     input = infile.read() # infile.readlines()

# implement passports as dictionaries on fields

# required fields
fields = ("byr","iyr","eyr","hgt","hcl","ecl","pid")

# parsing a passport


# split at empty lines:
blocks = re.split("\n *\n",input)

def blanks(s):
  b = True
  for c in s:
    b = b and (c == ' ' or c == '\n')
  return b

def parse_passport(s):
  pas = {}
  fs = [f for f in re.split(" |\n",s) if not blanks(f)]
  for f in fs:
    fn,fv = re.split(":",f)
    pas[fn] = fv
  return pas

# Part 1

def check_passport(pas):
  b = True
  fs = pas.keys()
  for f in fields: b = b and f in fs
  return b

passports = [parse_passport(b) for b in blocks]
valid_passports = [p for p in passports if check_passport(p)]

print("Part 1: " + str(len(valid_passports)))

# Part 2

print(passports[0])

def is_num(s):
  b = True
  for c in s: b = b and c in "0123456789"
  return b

def check_hgt(s):
  b = False
  if len(s) > 3:
    u = s[-2:]
    hs = s[0:-2]
    if is_num(hs):
      h = int(hs)
      if u == "cm":
            b = 150 <= h <= 193
      elif u == "in":
            b = 59 <= h <= 76
  return b

def check_fields(pas):
  # check year ranges
  b = 1920 <= int(pas["byr"]) <= 2002 \
      and 2010 <= int(pas["iyr"]) <= 2020 \
      and 2020 <= int(pas["eyr"]) <= 2030
  # check height
  print(pas["hgt"])
  b = check_hgt(pas["hgt"])
  # check hair color


  return b

valid_passports = [p for p in valid_passports if check_fields(p)]
print("Part 2: " + str(len(valid_passports)))
