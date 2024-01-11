class Graph:

    def __init__(self, n):
        self.adj = {}
        for i in range(n):
            self.adj[i] = []

    def has_edge(self, node1, node2):
        return node2 in self.adj[node1]

    def add_edge(self, node1, node2):
        if not self.has_edge(node1, node2):
            self.adj[node1].append(node2)
            self.adj[node2].append(node1)

    def get_size(self):
        return len(self.adj)

#Breadth First Search
def BFS(G, node1, node2):
    marked = {}
    Q = []
    for i in range(G.get_size()):
        marked[i] = False
    Q.append(node1)
    marked[node1] = True
    while len(Q) > 0:
        current_node = Q[0]
        print("Visiting node: " + str(current_node))
        Q = Q[1:]
        for node in G.adj[current_node]:
            if node == node2:
                return True
            if not marked[node]:
                Q.append(node)
                marked[node] = True
    return False

#Depth First Search
def DFS(G, node1, node2):
    S = [node1]
    marked = {}
    for node in G.adj:
        marked[node] = False
    while len(S) != 0:
        current_node = S.pop()
        if not marked[current_node]:
            print("Visiting node: " + str(current_node))
            marked[current_node] = True
            for node in G.adj[current_node]:
                if node == node2:
                    return True
                S.append(node)
    return False


G1 = Graph(10)
G1.add_edge(0, 1)
G1.add_edge(0, 4)
G1.add_edge(0, 7)
G1.add_edge(1, 2)
G1.add_edge(2, 3)
G1.add_edge(4, 5)
G1.add_edge(5, 6)
G1.add_edge(7, 8)
G1.add_edge(8, 9)

G2 = Graph(10)
G2.add_edge(0, 1)
G2.add_edge(0, 4)
G2.add_edge(0, 7)
G2.add_edge(1, 2)
G2.add_edge(2, 3)
G2.add_edge(4, 5)
G2.add_edge(5, 6)
G2.add_edge(7, 8)
G2.add_edge(8, 9)