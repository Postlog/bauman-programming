#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void revarray(void *base, unsigned long nel, unsigned long width)
{
	void *temp = malloc(width);
	
	for(int i = 0; i < nel / 2; i++)
	{
		void *lside = base + i * width, 
			 *rside = base + (nel - 1 - i) * width;

		memcpy(temp, lside, width);
		memcpy(lside, rside, width);
		memcpy(rside, temp, width);
	}
	
	free(temp);
}

char array[] = {
	10, 20, 30, 40, 50, 60
};

unsigned long size = 6;

int main(int argc, char **argv)
{
        revarray(array, size, sizeof(char));

        int i;
        for (i = 0; i < size; i++) {
                printf("%i\n", array[i]);
        }

        return 0;
}
