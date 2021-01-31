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

def check_passport(pas):
  b = True
  fs = pas.keys()
  for f in fields: b = b and f in fs
  return b

passports = [parse_passport(b) for b in blocks]

# Part 1
valid_passports = [p for p in passports if check_passport(p)]
print("Part 1: " + str(len(valid_passports)))



