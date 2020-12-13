#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void prefix(char *sub, int *p, int n)
{
	int j = 0;
	p[0] = 0;
	for(int i = 1; i < n; i++)
	{
		while(j > 0 && sub[j] != sub[i])
			j = p[j-1];
		if(sub[j] == sub[i])
			j++;
		p[i] = j;
	}
}


int KMP_all (char *str, char *sub, int **sub_idxs)
{
	int str_len = strlen(str), sub_len = strlen(sub);
	int p[sub_len];

	prefix(sub, p, sub_len);

	int *idxs = NULL,
		c = 0;
	for(int i = 0, j = 0; i < str_len; i++)
	{
		while(j > 0 && sub[j] != str[i])
			j = p[j - 1];
		if(sub[j] == str[i])
            j++;
		if(j == sub_len)
        {
        	idxs = (int*)realloc(idxs, sizeof(int) * ++c);
        	idxs[c - 1] = i - j + 1;
        }
	}

	*sub_idxs = idxs;
	return c;
}


int main(int argc, char **argv)
{
	char *sub = argv[1],
		 *word = argv[2];

	int *sub_idxs;
	int len = KMP_all(word, sub, &sub_idxs);

	for(int i = 0; i < len; i++)
	{
		printf("%i\n", sub_idxs[i]);
	}
	free(sub_idxs);
	return 0;
}