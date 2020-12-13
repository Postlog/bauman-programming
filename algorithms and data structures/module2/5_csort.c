#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int split(char *s, char ***w)
{
	char **words = NULL, *word = NULL, c = 1;
	unsigned long wordscount = 0, charscount = 0;
	while(c != '\0')
	{
		c = *(s++);
		if(c != ' ' && c != '\0')
		{
			if(charscount == 0) word = (char *)malloc(sizeof(char));
			else word = (char *)realloc(word, sizeof(char) * (charscount + 1));
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


void csort(char *src, char *dest) 
{
	char **words;
	int wcount = split(src, &words), null_term_idx = 0;

	int count[wcount];
	for(int i = 0; i < wcount; i++) count[i] = 0;

	for(int i = 0; i < wcount - 1; i++)
	{
		int l1 = strlen(words[i]);
		null_term_idx += l1 + 1;

		for(int j = i + 1; j < wcount; j++)
		{
			int l2 = strlen(words[j]);

			if(l1 > l2) count[i] += l2 + 1;
			else count[j] += l1 + 1;
		}
	}

	null_term_idx += strlen(words[wcount-1]);

	for(int i = 0; i < wcount; i++)
		strcpy(dest + count[i], words[i]);

	for(int i = 0; i < wcount; i++)
		if(count[i] != 0) dest[count[i] - 1] = ' ';

	dest[null_term_idx] = '\0';

	for(int i = 0; i < wcount; i++)
		free(words[i]);
	free(words);
}

int get_string(char **p)
{
	char *words = (char*)malloc(sizeof(char)), c;
	scanf("%c", &c);

	int slen = 0, tr_slen = -1;
	while(c != '\n')
	{
		words[slen] = c;
		words = (char*)realloc(words, sizeof(char) * (++slen + 1));
		scanf("%c", &c);
	}
	words[slen] = '\0';

	*p = words;
	return slen;
}

int main()
{
	char *string, c;
	int slen = get_string(&string);

	char sorted_string[slen];
	csort(string, sorted_string);

	printf("%s\n", sorted_string);
	free(string);
	return 0;
}