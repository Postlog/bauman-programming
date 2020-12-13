#include <stdio.h>
#include <locale.h>

unsigned long peak(unsigned long nel,
        int (*less)(unsigned long i, unsigned long j))
{
	unsigned long a = 0, b = nel, c = (a + b) / 2;
	while(a <= b)
	{	
		int l = c > 0 ? less(c, c - 1) : 0, 
			r = c < (nel - 1) ? less(c, c + 1) : 0;

		if (l == 0 && r == 0) return c;
		else if (l == 1 && r == 0) b = c - 1;
		else if (l == 0 && r == 1) a = c + 1;
		else b = c - 1;

		c = (a + 1) / 2 + b / 2;
	}

	return -1;
}


int array[] = {
	210,
	462,
	175,
	169
};

int less(unsigned long i, unsigned long j)
{
	return array[i] < array[j];
}

int main(int argc, char **argv)
{
	int i = peak(4, less);
	if ((i == 0 || array[i] >= array[i-1]) &&
		(i == 3 || array[i] >= array[i+1])) {
		printf("CORRECT, %i\n", i);
	} else {
		/* Если функция peak работает правильно,
		сюда никогда не будет передано
		управление! */
		printf("WRONG\n");
	}
	return 0;
}