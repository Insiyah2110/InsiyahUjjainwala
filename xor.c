#include <stdio.h>

int xor(int a, int b)
{
    return a^b;
}

extern int axor(int e, int f);

int main()
{
    int a = 12;
    int b = 25;

    printf("XOR of 12 and 25 is: %d ", xor(a,b));

    int c = 20;
    int d = 21;

    printf("\nXOR of 20 and 21 is: %d ", xor(c,d));

    return 0;
}
