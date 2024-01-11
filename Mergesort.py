def mergeSort(L):
    if len(L) <= 1:
        
        return L
    
    else:
        
        mid = len(L)//2
        left = L[0:mid]
        right = L[mid:len(L)]

        mergeSort(left)
        mergeSort(right)

        x = 0
        y = 0
        z = 0

        while x < len(left) and y < len(right):
            
            if left[x] > right[x]:
                L[z] = right[y]
                y += 1
            
            else:
                L[z] = left[x]
                x += 1
            z += 1

        while len(left) > x:
            L[z] = left[x]
            x,z = x+1 , z+1

        while len(right) > y:
            L[z] = right[y]
            y,z = y+1, z+1

    return L

def mergeSort_exp(n, step):
    times = []
    indicies = []
    for i in range(step, n, step):
        total = 0
        for _ in range(100):
            print("Testing Merge Sort on size " + str(i))
            L = create_random_list(i, i)
            start = timeit.default_timer()
            mergeSort(L)
            end = timeit.default_timer()
            total += end - start
        average = total / n
        times.append(average)
        indicies.append(i)
    return [times,indicies]

n = 5000
step = 100
mergeSortResults = mergeSort_exp(n, step)
buMergeSortResults = buMergeSort_exp(n, step)
print(mergeSortResults)
print(mergeSortResults2)
plot.plot(mergeSortResults[1],mergeSortResults[0], label='Merge Sort')
plot.plot(mergeSortResults2[1],mergeSortResults2[0], label='Bottom-Up Merge Sort')
plot.xlabel('List Size')
plot.ylabel('Time Taken (s)')
plot.title("Sorting Algorithm Test")
plot.legend()
plot.show()
