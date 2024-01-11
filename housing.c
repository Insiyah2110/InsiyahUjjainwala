#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>


// Typedef structure for New City Housing Price 
typedef struct CityHousing {
  char city[100];
  int price;
} City;


// Function used to output city and the average house price corresponding to it.
void display(City * array , int maximum){
  for (int i = 0 ; i < maximum ; i++) {
    printf ("City: %s Price: %d \n" , array[i].city , array[i].price);
  }
}

// Selection sort function. Reference: https://www.geeksforgeeks.org/selection-sort/
void swap(City *first, City *second) {
  City temp = *first;
  *first = *second;
  *second = temp;
}

void selectionSort(City * array , int max) {
  int min_idx;
  for (int step = 0; step < max - 1; step++) {
    min_idx = step;
    for (int i = step + 1; i < max; i++) {
      if (array[i].price < array[min_idx].price){
        min_idx = i;
        }
    }
    swap(&array[min_idx], &array[step]);
  }
}


int main(void) {
    // Defined variables, pointers etc.
    char buffer_size[1023];
    City * dynamic_array = NULL;
    dynamic_array = malloc (sizeof(City));
    City * new_array = NULL;
    new_array = malloc(sizeof(City));

    int size = 1;
    
    char input;
    int house_price;

    char choice ='y';
    char user_choice;

    // User input
    while (choice != 'n') {
        int i = 0 ;
        printf("Enter the name of the city: ");
        while ((input = getchar()) != '\n' ) buffer_size[i++] = input;
        buffer_size[i] = '\0';

        printf("Enter the average house price: ");
        scanf("%d" , &house_price);

        City duplicate ; 
        strcpy(duplicate.city , buffer_size);
        duplicate.price = house_price ; 

        dynamic_array[size-1] = duplicate;

        printf("Enter another city? (y/n): ");
        getchar();
        scanf("%c", &user_choice);
        getchar();

        if (user_choice == 'y') {
          new_array = (City*) realloc(dynamic_array , ((size+1) * sizeof(City))); // Reallocating size
          size+=1;
          dynamic_array = new_array;
        } 
        else if (user_choice == 'n') {
          choice = user_choice;
          break ; 
        } 
    }

    // Outputting the cities and average house prices using the display function defined above.
    printf("\nList of city and prices: \n \n") ;
    display(dynamic_array , size) ;

    // Outputting the data after sorting it using the selection sort function defined above.
    selectionSort(dynamic_array , size);
    printf("\n Sorted: \n \n");
    display(dynamic_array , size) ;

    // Printing the city with the highest property price.
    printf("\nThe city with the highest house price: %s\n" , dynamic_array[size-1].city);

    // Freeing the array as per the question.
    free(dynamic_array);
    return 0;
    }