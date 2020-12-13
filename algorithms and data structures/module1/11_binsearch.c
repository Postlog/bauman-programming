#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned long binsearch(unsigned long nel, int (*compare)(unsigned long i))
{
	unsigned long a = 0, b = nel - 1, c = (b + a) / 2;
	while (a <= b)
	{
		int cresult = compare(c);

		if (cresult == -1) a = c + 1;
		else if (cresult == 1) b = c - 1;
		else return c;

		c = (b + a) / 2;
	}

	return nel;
}

int array[] = { 1, 2, 30, 45, 50, 51, 55, 60 };
const int k = 60;

int compare(unsigned long i)
{
        if (array[i] == k) return 0;
        if (array[i]  < k) return -1;
        return 1;
}

int main(int argc, char  **argv)
{
        printf("%lu\n", binsearch(8, compare));
        return 0;
}