# Advent of Code 2020, Day 4

import re

with open("../input04", "r") as infile:
     input = infile.read()

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

def is_num(s):
  b = True
  for c in s: b = b and c in "0123456789"
  return b


# check for a year between two values 
def check_year(mn,mx,s):
  b = False
  if len(s)==4 and is_num(s):
    b = mn <= int(s) <= mx
  return b

# check for a correct height in centimeters or inches
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

def is_hex(s):
  b = True
  for c in s: b = b and c in "0123456789abcdef"
  return b

# check hair color
def check_hcl(s):
  b = False
  if len(s) == 7:
    b = s[0] == '#' and is_hex(s[1:])
  return b

# check eye color
def check_ecl(s):
  return s in ["amb","blu","brn","gry","grn","hzl","oth"]

# check passport ID
def check_pid(s):
  return len(s) == 9 and is_num(s)

def check_fields(pas):
  b = check_year(1920,2002,pas["byr"]) and \
      check_year(2010,2020,pas["iyr"]) and \
      check_year(2020,2030,pas["eyr"]) and \
      check_hgt(pas["hgt"]) and \
      check_hcl(pas["hcl"]) and \
      check_ecl(pas["ecl"]) and \
      check_pid(pas["pid"])
  return b

valid_passports = [p for p in valid_passports if check_fields(p)]
print("Part 2: " + str(len(valid_passports)))
