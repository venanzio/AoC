class Source:
  def __init__(self,s):
    self.text = s
    self.error = ''

def space(s):
  s.text = s.text.strip()

def word(s,w):
  space(s)
  n = len(w)
  if s.text[:n] == w:
    s.text = s.text[n:]
    return w
  else:
    s.error += "no parsing of '"+w+"'"
    return None

def num(s):
  space(s)
  sn = ''
  i = 0
  while s.text[i:]!='' and s.text[i].isdigit():
    sn += s.text[i]
    i += 1
  s.text = s.text[i:]
  space(s)
  if sn == '':
    s.error = 'failed to parse a number'
    return None
  else:
    return int(sn)

def new_line(s):
  return None
