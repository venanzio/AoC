class Source:
  def __init__(self,s):
    self.source = s
    self.error = ''

def space(s):
  s.source = s.source.strip()

def parse_word(s,w):
  space(s)
  n = len(w)
  if s.source[:n] == w:
    s.source = s.source[n:]
    return w
  else:
    s.error += "no parsing of '"+w+"'"
    return None

def parse_num(s):
  space(s)
  sn = ''
  i = 0
  while s.source[i:]!='' and s.source[i].isdigit():
    sn += s.source[i]
    i += 1
  s.source = s.source[i:]
  space(s)
  if sn == '':
    s.error = 'failed to parse a number'
    return None
  else:
    return int(sn)

s = Source(" wa 345  one two three")

print(s.source)
print(parse_num(s))
print(s.error)
