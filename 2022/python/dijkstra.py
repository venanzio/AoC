# Dijkstra's algorithm
# Venanzio Capretta, 2023

# a graph is represented as a adjacencey list
#  dictionary vertex -> [vertex]

# minimum of a dictionary with None values

def min_dic(dic):
  l = [(v,dic[v]) for v in dic if dic[v] != None]
  x = min(l, key = lambda vd: vd[1])
  return x

def dijkstra(gr,s,t):
  vertices = {v:None for v in gr.keys()}
  vertices[s] = 0
  visited = { }
  while vertices:
    (u,du) = min_dic(vertices)
    visited[u] = vertices.pop(u) 
    for v in set(vertices) & set(gr[u]):
      pass
  return None


gr = {1:[2,3,4], 2:[1,4], 3:[], 4:[2]}

print(dijkstra(gr,1,4))
