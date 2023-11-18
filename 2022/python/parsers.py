class Source:
  def __init__(self,s):
    self.text = s
    self.error = ''

  def next(self,s):
    return self.text[0]

# parse tokens

# empty space: returns True if source is empty
def space(s):
  s.text = s.text.strip()
  return (s.text == '')

def token(pr,s):
  space(s)
  return pr(s)

def char(s):
  if s.text == '':
    return None
  c = s.text[0]
  s.text = s.text[1:]
  return c

# parsing a given string 

def tok_word(w,s):
  n = len(w)
  if s.text[:n] == w:
    s.text = s.text[n:]
    return w
  else:
    return None

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

# parse a (non-empty) list of items separated by commas, each parsed by pr

def ne_list(pr,s):
  txt = s.text
  l = []
  x = pr(s)
  while x != None and word(',',s)==',':
    l.append(x)
    x = pr(s)
  if x == None:
    s.text = txt
    return None
  else:
    l.append(x)
    return l

def list(pr,s):
  l = []
  x = pr(s)
  while x != None and word(',',s)==',':
    l.append(x)
    x = pr(s)
  if x == None:
    s.text = txt
    return None
  else:
    l.append(x)
    return l
