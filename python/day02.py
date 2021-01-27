# Advent of Code 2020, Day 2

with open("../input02", "r") as infile:
     input = infile.readlines()

linp = [i.split("\n")[0] for i in input]

line = linp[0]
print(line)

spl = line.split(' ')
print(spl)
ns = spl[0].split('-')
min = int(ns[0])
max = int(ns[1])
ch = spl[1].split(':')[0]
word = spl[2]

print(min,max,ch,word)




valid_2 = []
valid_1 = []
for lin in linp:
    key, value = lin.split(": ")
    char = key.split(" ")[1]
    # print(char, key.split(" ")[0].split("-"))
    n1, n2 = key.split(" ")[0].split("-")
    n1, n2 = int(n1)-1, int(n2)-1

    if n1+1<=value.count(char)<=n2+1:
        valid_1.append(lin)

    c = 0
    try:

        if value[int(n1)] == char:
            c+=1
        if value[int(n2)] == char:
            c+=1
        if c == 1:
            valid_2.append(lin)
    except:
        pass
print(" Solution 1: {len(valid_1)}\nSolution 2:{len(valid_2)}")

