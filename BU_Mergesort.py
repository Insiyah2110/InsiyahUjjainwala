def merge(L, start, mid, end):
    n1 = mid - start + 1
    n2 = end - mid
    
    left = [0] * n1 
    right = [0] * n2
    
    for i in range(0, n1): 
        left[i] = L[start + i]
        
    for j in range(0, n2): 
        right[j] = L[mid + j + 1] 
  
    i = 0
    j = 0
    k = start
    
    while n1 > i and n2 > j: 
        if left[i] > right[j]: 
            L[k] = right[j] 
            j += 1
        else: 
            L[k] = left[i] 
            i += 1
        k += 1
  
    while n1 > i: 
        L[k] = left[i] 
        i += 1
        k += 1
  
    while n2 > j: 
        L[k] = right[j] 
        j += 1
        k += 1
        
def buMergeSort(L):
    
    window_size = 1
    
    while window_size < len(L):
        for i in range(0, len(L), window_size * 2):
            mid = min(i + window_size - 1, len(L) - 1)
            end = min(i + (window_size * 2 - 1), len(L) - 1)
            merge(L, i, mid, end)
        window_size *= 2

    return L

print(buMergeSort([5,4,3,2,1]))

'''def buMergeSort_exp(n, step):
    times = []
    indicies = []
    for i in range(step, n, step):
        total = 0
        for _ in range(100):
            print("Testing Bottom-Up Merge Sort on size " + str(i))
            L = create_random_list(i, i)
            start = timeit.default_timer()
            buMergeSort(L)
            end = timeit.default_timer()
            total += end - start
        average = total / n
        times.append(average)
        indicies.append(i)
    return [times,indicies]

n = 5000
step = 100
buMergeSortResults = buMergeSort_exp(n, step)
print(mergeSortResults2)
plot.plot(mergeSortResults2[1],mergeSortResults2[0], label='Bottom-Up Merge Sort')
plot.xlabel('List Size')
plot.ylabel('Time Taken (s)')
plot.title("Sorting Algorithm Test")
plot.legend()
plot.show()'''

