#include <stdio.h>
#include <stdlib.h>

void representation(){
    char* array_byte = malloc(8); //Creating a char array to store 8 bytes.
    int *int_pointer = (int*) array_byte; //Interpreting the byte array pointer as an integer pointer.
    *int_pointer = 0x7F;

    // If 0x7F is stored in sign magnitude, then the most significant bit (left most) represents the sign, and the rest of the bits represent the integer.
    // 7F is stored as an 8-bit integer, so it is stored as 01111111. 
    // This integer would be stored as 127 in signed magnitude as 0 would represent positive sign and the remaining bits would evaluate to 127. 
    if (array_byte[0] == 127) {
        printf("This integer is stored in sign magnitude\n");
    }

    // 0x7F is equal to 01111111 in binary.
    // If this integer is stored in two's complement, then after inverting the bits and adding 1, we would get 10000001.
    // Therefore, this integer would be stored as -1 in two's complement.
    else if (array_byte[0] == 1){
        printf("This integer is stored in two's complement\n");
    }
}

int main () {

    char* array_byte = malloc(8); //Creating a char array to store 8 bytes.
    int *int_pointer = (int*) array_byte; //Interpreting the byte array pointer as an integer pointer.
    *int_pointer = 0x04030201; //Storing the integer in the memory of the integer pointer.

    //Printing the integer to the screen to check if it is stored as Big endian or Little endian.
    int j = 0;
    while (j<4){
        printf("%X", array_byte[j]);
        printf("\n");
        j++;
    }

    // If the first digit is 04, then it is stored as 4 3 2 1, hence making it Big Endian.
    if (array_byte[0] == 04){
        printf("Integer is stored in Big Endian\n");
    }
    // If the first digit is not 04, then it can only be 01, i.e. the number is stored as 1 2 3 4, making it Little Endian.
    else {
        printf("Integer is stored in Little Endian\n");
    }

    representation();

    return 0;
}