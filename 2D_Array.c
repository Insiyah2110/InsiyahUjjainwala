#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

char *arr_ptr;
int i, j, row, column;

char* two_d_alloc(size_t row, size_t column, size_t size){

    char *arr_ptr = malloc(row * column * size);
    return arr_ptr;

}

void dealloc(char *arr_ptr){
    free(arr_ptr);
}

int two_d_store_int(int value, char *arr_ptr, size_t i, size_t j, size_t row, size_t column){

    if (arr_ptr != NULL && i >= 0 && j >= 0 && row >= 0 && column >= 0){
        arr_ptr[i * column + j] = value;
    }

    else{
        return -1;
    }

    return 0;
}

int two_d_fetch_int(char* arr_ptr, size_t i, size_t j, size_t row, size_t column){
    int value;
    if (arr_ptr != NULL && i >= 0 && j >= 0 && row >= 0 && column >= 0){
        value = arr_ptr[i * column + j];
        return value;
    }

    else{
        printf("Error");
    }

    return 0;
}

int two_d_store(void *value, char *arr_ptr, size_t i, size_t j, size_t row, size_t column, size_t size){
    if (arr_ptr != NULL && i >= 0 && j >= 0 && row >= 0 && column >= 0){
        memcpy((arr_ptr + (i * column + j) * size), value, size);
    }
    else{
        return -1;
    }
    return 0;
}

void* two_d_fetch(char *arr_ptr, size_t i, size_t j, size_t row, size_t column, size_t size){
    return (arr_ptr + (i * column + j) * size);  
}


int main(){
    row = 3;
    column = 3;
    int val;

    char* arr_ptr = two_d_alloc(row, column, sizeof(int));

    //Testing 2D Array of integers.
    for (i = 0; i < row; i++){
        for (j = 0; j < column; j++){
            val = i+j;
            two_d_store_int( val, arr_ptr, i, j, row, column);
        }
    }

      
    for (i = 0; i < row; i++){
        for (j = 0; j < column; j++){
            printf("%d \t",two_d_fetch_int(arr_ptr, i, j, row, column));
        }
        printf("\n");
    }

    printf("\n\n");

    //Boundary case
    //Though we are allocating space for a 3x3 array, accessing the 3x3 element yields value 0
    //Since the array is 0 indexed.
    printf("%d \n \n ",two_d_fetch_int(arr_ptr, 3, 3, row, column));
    //Same logic is applied to all elements of the matrix. To access the ij value, we need the (i-1)(j-1)
    //element of the array.

    //Testing 2D Array of floats.
    float rand_float = 2.67;
    float *arg;
    arg = &rand_float;

    for (i = 0; i < row; i++){
        for (j = 0; j < column; j++){
            two_d_store( arg, arr_ptr, i, j, row, column, sizeof(float));
        }
    }

    for (i = 0; i < row; i++){
        for (j = 0; j < column; j++){
            printf("%.2f \t", *((float*)two_d_fetch(arr_ptr, i, j, row, column, sizeof(float))));
        }
        printf("\n");
    }
    return 0;

    dealloc(arr_ptr);
}