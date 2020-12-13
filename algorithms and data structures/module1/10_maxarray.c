#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int maxarray(void  *base, unsigned long nel, unsigned long width,
        int (*compare)(void *a, void *b))
{
	void *max = malloc(width);
	int maxidx = 0;
	
	memcpy(max, base, width);

	for (int i = 1; i < nel; i++)
	{
		void *item = (base + i * width);
		if (compare(max, item) < 0) 
		{
			memcpy(max, item, width);
			maxidx = i;
		}
	}

	free(max);
	return maxidx;
}

unsigned char array[] = {
	153,
	1,
	15,
	191,
	232,
	251,
	27,
	174,
	26,
	3,
	68,
	48
};

int compare(void *a, void *b)
{
	return (int)(*(unsigned char*)a) - (int)(*(unsigned char*)b);
}

int main(int argc, char **argv)
{
	printf("%d\n", maxarray(array, 12, sizeof(unsigned char), compare));
	return 0;
}