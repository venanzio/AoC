# Advent of Code 2020, Day 3

with open("../input03", "r") as infile:
     input = infile.readlines()

# divide the input into lines
lines = [i.split("\n")[0] for i in input]



# implement passports as dictionaries on fields

# required fields
fields = ("byr","iyr","eyr","hgt","hcl","ecl","pid")

passport = {"ecl": "gry", "pid": "860033327", "eyr": "2020", 
            "hcl": "#fffffd", "byr": "1937", "iyr": "2017",
            "cid": "147", "hgt": "183cm"}

def check_passport(pas):
  b = True
  fs = pas.keys()
  for f in fields: b = b and f in fs
  return b


for f in passport.keys(): print(f + ": " +  passport[f])

if check_passport(passport): 
  print('valid')
else: 
  print('invalid')


