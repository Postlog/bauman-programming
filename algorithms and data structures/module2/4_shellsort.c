#include <stdlib.h> 
#include <stdio.h> 
#include <math.h>

int fib(int nel)
{
	int a = 0, b = 1, s = 0;

	while(a + b < nel)
	{
		s = a + b;
		a = b;
		b = s;
	}
	return s;
}

int prev_fib(int fib)
{
	if(fib == 1) return 0;
	
	double prev_fib = fib / ((1 + sqrt(5)) / 2.0); 
	return (int)round(prev_fib);
}


void shellsort(unsigned long nel, 
        int (*compare)(unsigned long i, unsigned long j), 
        void (*swap)(unsigned long i, unsigned long j)) 
{ 
	int d = fib(nel);
	while(d >= 1)
	{
		for(int i = d; i < nel; i++)
		{
			int loc = i;
			while(loc - d >= 0 && compare(loc - d, loc) > 0)
			{
				swap(loc, loc - d);
				loc -= d;
			}
		}
		d = prev_fib(d);
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
 
        shellsort(n, compare, swap); 
        for (i = 0; i < n; i++) printf("%d ", array[i]); 
        printf("\n"); 
 
        free(array); 
        return 0; 
}