class Source:
  def __init__(self,s):
    self.text = s
    self.error = ''

  def next(self):
    if self.text == '':
      return None
    else:
      return self.text.strip()[0]

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

def integer(s):
  if word('-',s) == None:
    word('+',s)  # ignored if not there
    return num(s)
  else:
    return (-num(s))

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

# parsing with a sequence of parsers
def seq(lpr,s):
  txt = s.text
  l = []
  for pr in lpr:
    x = pr(s)
    if x == None:
      s.text = txt
      return None
    else:
      l.append(x)
  return l

# parse a potentially empty list
def lst(pr,s):
  l = []
  x = pr(s)
  while x != None:
    l.append(x)
    cx = seq([lambda s: word(',',s), pr],s)
    if cx != None:
      x = cx[1]
    else:
      x = None
  return l


# parse a potentially empty list with given separator
def lst_sep(pr,sep,s):
  l = []
  x = pr(s)
  while x != None:
    l.append(x)
    cx = seq([lambda s: word(sep,s), pr],s)
    if cx != None:
      x = cx[1]
    else:
      x = None
  return l


def pair(pr1,pr2,s):
  txt = s.text
  l = seq([pr1,
           lambda s: word(',',s),
           pr2],s)
  if l == None:
    return None
  else:
    return (l[0],l[2])



