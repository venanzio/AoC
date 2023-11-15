class Source:
  def __init__(self,s):
    self.text = s
    self.error = ''

# parse tokens

def space(s):
  s.text = s.text.strip()

def token(pr,s):
  space(s)
  return pr(s)

def char(s):
  c = s.text[0]
  s.text = s.text[1:]
  return c

# parsing a given string 

def tok_word(w,s):
  n = len(w)
  if s.text[:n] == w:
    s.text = s.text[n:]
    return True
  else:
    s.error += "no parsing of '"+w+"'"
    return False

def word(w,s):
  return token(lambda s: tok_word(w,s),s)

# parsing a natural number

def tok_num(s):
  sn = ''
  i = 0
  while s.text[i:]!='' and s.text[i].isdigit():
    sn += s.text[i]
    i += 1
  s.text = s.text[i:]
  if sn == '':
    s.error = 'failed to parse a number'
    return None
  else:
    return int(sn)

def num(s):
  space(s)
  return tok_num(s)

# skip to the next line (if there is any)

def newline(s):
  n = s.text.find('\n')
  if n==-1:
    return False
  else:
    s.text = s.text[n+1:]
    return True

# parse a list of items, each parsed by pr

def list(s,pr):
  l = [pr(s)]
  while s.text != '' and s.text[0] == ',':
    word(s,',')
    l.append(pr(s))
  return l


