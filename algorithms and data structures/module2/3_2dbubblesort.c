#include <stdlib.h>
#include <stdio.h>

void bubblesort(unsigned long nel, 
        int (*compare)(unsigned long i, unsigned long j), 
        void (*swap)(unsigned long i, unsigned long j)) 
{ 
        int left = 0, right = nel - 1, swapped = 1,
                new_left = left,
                new_right = right;
        while(swapped)
        {
        	swapped = 0;
        	for(int i = left; i < right; i++)
        	{
        		if(compare(i, i + 1) > 0)
        		{
        			swap(i, i + 1);
                                swapped = 1;
                                new_right = i;
        		}
        	}

                right = new_right;
        	if(swapped == 0) break;
        	swapped = 0;

        	for(int i = right; i > left; i--)
        	{
        		if(compare(i - 1, i) > 0)
        		{
        			swap(i - 1, i);
        			swapped = 1;
                                new_left = i;
        		}
        	}
        	left = new_left;
        }
}


int *array;

int compare(unsigned long i, unsigned long j)
{
	if (i <= j) {
		printf("COMPARE %d %d\n", i, j);
	} else {
		printf("COMPARE %d %d\n", j, i);
	}

	if (array[i] == array[j]) return 0;
	return array[i] < array[j] ? -1 : 1;
}

void swap(unsigned long i, unsigned long j)
{
	if (i <= j) {
		printf("SWAP %d %d\n", i, j);
	} else {
		printf("SWAP %d %d\n", j, i);
	}

	int t = array[i];
	array[i] = array[j];
	array[j] = t;
}

int main(int argc, char **argv)
{
	int i, n;
	scanf("%d", &n);

	array = (int*)malloc(n * sizeof(int));
	for (i = 0; i < n; i++) scanf("%d", array+i);

	bubblesort(n, compare, swap);
	for (i = 0; i < n; i++) printf("%d ", array[i]);
	printf("\n");

	free(array);
	return 0;
}