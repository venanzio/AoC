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

def parse_passport(s):
  pas = {}
  for fs in re.split(" +|\n+",s):
    fn,fv = re.split(":",fs)
    pas[fn] = fv
  return pas

def check_passport(pas):
  b = True
  fs = pas.keys()
  for f in fields: b = b and f in fs
  return b

#passports = [parse_passport(b) for b in blocks]
#print(passports[0])

passport = parse_passport(blocks[0])

for f in passport.keys(): print(f + ": " +  passport[f])

if check_passport(passport): 
  print('valid')
else: 
  print('invalid')


