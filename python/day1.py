with open('../input01','r') as infile:
     input = infile.read()

numbers = [int(s) for s in input.split() if s.isdigit()]
numbers.sort()


# find two elements (with indices between i and j) that add up to s
def sum2(i,j,s):
  sm = numbers[i] + numbers[j]
  while i < j and sm != s:
    if   sm < s: i += 1
    else       : j -= 1
    sm = numbers[i] + numbers[j]
  return (i,j)

print("Part 1")
(fst,snd) = sum2(0,len(numbers)-1,2020)
print (numbers[fst],numbers[snd],numbers[fst]*numbers[snd])

# find three elements (with indices between i and j) that add up to s
def sum3(i,j,s):
  sm = -1
  while i < j-1 and sm != s:
    x = i
    i += 1
    (y,z) = sum2(i,j,s-numbers[x])
    sm = numbers[x] + numbers[y] + numbers[z]
  return (x,y,z)

print("Part 2")
(fst,snd,trd) = sum3(0,len(numbers)-1,2020)
print (numbers[fst],numbers[snd],numbers[trd],
       numbers[fst]*numbers[snd]*numbers[trd])
