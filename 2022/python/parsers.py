class Source:
  def __init__(self,s):
    self.text = s
    self.error = ''

def space(s):
  s.text = s.text.strip()

def char(s):
  c = s.text[0]
  s.text = s.text[1:]
  return c

def word(s,w):
  space(s)
  n = len(w)
  if s.text[:n] == w:
    s.text = s.text[n:]
    return True
  else:
    s.error += "no parsing of '"+w+"'"
    return False

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

def newline(s):
  n = s.text.find('\n')
  s.text = s.text[n+1:]
  return None

def list(s,pr):
  space(s)
  l = [pr(s)]
  while s.text != '' and s.text[0] == ',':
    word(s,',')
    l.append(pr(s))
  return l


