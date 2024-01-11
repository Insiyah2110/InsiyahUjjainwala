class DisjointSet:

    def __init__(self, values):
        self.parents = {}
        self.size = {}
        for value in values:
            self.parents[value] = value
            self.size[value] = 1

    def find(self, value):
        if self.parents[value] == value:
            return value
        self.parents[value] = self.find(self.parents[value])
        return self.parents[value]

    def union(self, value1, value2):
        root1 = self.find(value1)
        root2 = self.find(value2)
        if root1 == root2:
            return

        if self.size[root1] >= self.size[root2]:
            self.parents[root2] = root1
            self.size[root1] += self.size[root2]
        else:
            self.parents[root1] = root2
            self.size[root2] += self.size[root1]

    def __str__(self):
        sets = {}
        L = []
        for value in self.parents:
            if self.find(value) in sets:
                sets[self.find(value)].append(value)
            else:
                sets[self.find(value)] = [value]
        for s in sets:
            L.append(sets[s])
        return str(L)

    def __repr__(self):
        return self.__str__()
