#include <stdlib.h>
#include <stdio.h>

int split(char *s, char ***w)
{
	char **words = NULL, *word = NULL, c = 1;

	unsigned long wordscount = 0, charscount = 0;
	
	while(c != '\0')
	{
		c = *(s++);

		if(c != ' ' && c != '\0')
		{
			if(charscount == 0)
				word = (char *)malloc(sizeof(char));
			else
				word = (char *)realloc(word, sizeof(char) * (charscount + 1));

			*(word + charscount) = c;
			charscount++;
		}
		else if(charscount != 0)
		{
			word = (char *)realloc(word, sizeof(char) * (charscount + 1));
			word[charscount] = '\0';	
			charscount = 0;

			words = (char **)realloc(words, sizeof(char *) * (wordscount + 1));
			*(words + wordscount) = word;
			wordscount++;
		}
	}

	*w = words;
	return wordscount;
}


int main()
{
	char *word = "a b c d";
	char **words;
	int c = split(word, &words);
	for(int i = 0; i < c; i++)
	{
		printf("\"%s\"\n", words[i]);
	}
	return 0;
}