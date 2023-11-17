# Dijkstra's algorithm
# Venanzio Capretta, 2023

# a graph is represented as a adjacencey list
#  dictionary { vertex:[vertex] }

# minimum of a dictionary with None values

def min_dic(dic):
  l = [(v,dic[v]) for v in dic if dic[v] != None]
  if l == []:
    return None
  x = min(l, key = lambda vd: vd[1])
  return x

# shortest paths to every node starting from s
def all_shortest(gr,s):
  vertices = {v:None for v in gr.keys()}
  vertices[s] = 0
  visited = { }
  while min_dic(vertices) != None:
    (u,du) = min_dic(vertices)
    visited[u] = vertices.pop(u) 
    for v in set(vertices) & set(gr[u]):
      if vertices[v] == None:
        vertices[v] = du+1
      else:
        vertices[v] = min(vertices[v],du+1)
  return visited

# shortest path from s to t
def shortest(gr,s,t):
  return all_shortest(gr,s)[t]
