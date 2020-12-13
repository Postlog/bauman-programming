#include  <stdio.h>

int strdiff(char *a, char *b)
{
	unsigned long c = 0, i = 0;
	char ca = 1, cb = 1;
	
	while (ca && cb)
	{
		ca = *(a + i);
		cb = *(b + i);
		
		for (int j = 0; j < 8; j++)
		{
		
			if (((ca >> j) & 1) != (((cb >> j) & 1)))
				return c;
			c++; 
		}
		i++;
	}

	return ca || cb ? c : -1;
}

int main(int argc, char **argv)
{
        char s1[1000], s2[1000];
        gets(s1);
        gets(s2);
        printf("%d\n", strdiff(s1, s2));

        return 0;
}