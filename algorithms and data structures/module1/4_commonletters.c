#include <stdio.h>

int main()
{
	char c;
	unsigned long chars1 = 0, chars2 = 0, *pchars = &chars1, cchars;
	
	scanf("%c", &c);
	while (c != '\n')
	{
		if (c == ' ') pchars = &chars2;
		else *pchars |= (1L << (c - 'A'));
		scanf("%c", &c);
	}

	cchars = chars1 & chars2;
	for (char i = 0; i <= ('z' - 'A'); i++)
	{
		if (cchars % 2) printf("%c", 'A' + i);
		cchars /= 2;
	}
	printf("\n");
	return 0;
}