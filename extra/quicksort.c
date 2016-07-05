#include <stdio.h>

void
printArray(int* array, int length)
{
        for (int i = 0; i < length; ++i)
        {
                printf("%d ", array[i]);
        }
}

void
swap(int* a, int* b)
{
        int temp = *a;
        *a = *b;
        *b = temp;
}

int
partition(int* array, int length)
{
        int pivot = array[length / 2];
        int low = 0;
        int high = length - 1;

        for (low = 0, high = length - 1;; ++low, --high)
        {
                while (array[low] < pivot)
                {
                        ++low;
                }
                while (array[high] > pivot)
                {
                        --high;
                }
                if (low >= high)
                {
                        break;
                }
                swap(&array[low], &array[high]);
        }
        return low;
}

void
quicksort(int* array, int length)
{
        if (length < 2) return;

        int pivot = partition(array, length);

        printf("\nQuicksort:    ");
        printArray(array, length);
        
        quicksort(&array[0], pivot);
        quicksort(&array[pivot], length - pivot);
}

int
main(int argc, char** argv)
{
        int array[] = { 13, 1, 3, 17, 4, 9 ,10, 12, 5, 6, 2 };
        int length = sizeof array / sizeof array[0];

        printf("Array before: ");
        printArray(array, length);
        
        quicksort(array, length);

        printf("\nArray after:  ");
        printArray(array, length);
        printf("\n");
        
        return 0;
}
