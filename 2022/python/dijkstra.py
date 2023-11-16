# Dijkstra's algorithm
# Venanzio Capretta, 2023

# a graph is represented as a adjacencey list
#  dictionary vertex -> [vertex]

# minimum of a dictionary with None values

def min_dic(dic):
  x = min(dic, key=dic.get)
  return (x, dic[x])

def dijkstra(gr,s,t):
  vertices = {v:None for v in gr.keys()}
  print(vertices)
  print(min(vertices.values()))

  return None


print(min_dic({1:20,2:24,3:7}))

gr = {1:[2,3,4], 2:[1,4], 3:[], 4:[2]}

print(dijkstra(gr,1,4))
