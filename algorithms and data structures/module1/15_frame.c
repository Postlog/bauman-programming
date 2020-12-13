#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char **args)
{
	if (argc != 4)
	{
		printf("Usage: frame <height> <width> <text>\n");
		return 0;
	}
	
	int height = atoi(args[1]),
		width = atoi(args[2]);

	char *word = args[3];
	int len = strlen(word);

	if (len > width - 2 || height < 3)
	{
		printf("Error\n");
		return 0;
	}

	int wordrow = (height + 1) / 2,
		wordstart = (width - len) / 2 + 1,
		wordend = wordstart + len - 1;

	for (int i = 1; i <= height; i++)
	{
		for (int j = 1; j <= width; j++)
		{
			if (i == 1 || i == height) {
				printf("*");
				if (j == width) printf("\n");
				continue;
			}

			if (j == 1 || j == width)
			{
				printf("*");
				if (j == width) printf("\n");
				continue;
			}

			if (i == wordrow)
			{
				if (j >= wordstart && j <= wordend)
				{
					printf("%c", word[j - wordstart]);
				}
				else printf(" ");
			}
			else printf(" ");
		}
	}
	return 0;

}