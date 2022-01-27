# Advent of Code 2017 - day 4
println("Advent of Code 2017, day 4")

f = open("input4")
input = readlines(f)
close(f)

# Part 1

function no_duplicates(l)
  length(l) == length(unique(l))
end

valid1 = length([l for l in input if no_duplicates(split(l))])

println("part 1: ", valid1)

# Part 2

function no_anagram(l)
  no_duplicates(map(w -> sort(collect(w)), l))
end

valid2 = length([l for l in input if no_anagram(split(l))])

println("part 2: ", valid2)
