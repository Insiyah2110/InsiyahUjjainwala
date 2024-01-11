def selection_sort(L):
    for i in range(len(L)):
        min_index = find_min_index(L, i)
        max_index = find_max_index(L,i)
        x = 0
        y = len(L) - 1
        while (x<y):
            swap(L,i,min_index)
            if L[min_index] != L[max_index]:
                swap(L,len(L)-1-i,max_index)
            x+=1
            y-=1
            
        
    return L


'''def swap_improved(L,i,j,x,y):
    while(i<x):
        L[i], L[j] = L[j], L[i]
        L[x], L[y] = L[y], L[x]'''

def swap(L,i,j):
    L[i],L[j] = L[j],L[i]


def find_min_index(L, n):
    min_index = n
    for i in range(n+1, len(L)):
        if L[i] < L[min_index]:
            min_index = i
    return min_index


def find_max_index(L,n):
    max_index = n
    for i in range(n+1, len(L)):
        if L[i] > L[max_index]:
            max_index = i
    return max_index


L = [3,65,1,33,2,7,34,0,9]

print(selection_sort(L))
