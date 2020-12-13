#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swap(void *a, void *b, size_t width)
{
	void *temp = malloc(width);
	memcpy(temp, a, width);
	memcpy(a, b, width);
	memcpy(b, temp, width);
	free(temp);
}

void heapify(void *base, int i, size_t n, size_t width,
		int (*compare)(const void *a, const void *b))
{
	while(1)
	{
		int l = 2 * i + 1,
			r = l + 1,
			j = i;

		if(l < n && compare(base + l * width, base + i * width) > 0)
			i = l;
		if(r < n && compare(base + r * width, base + i * width) > 0)
			i = r;
		if(i == j) break;

		swap(base + j * width, base + i * width, width);
	}
}

void build_heap(void *base, size_t nel, size_t width,
		int (*compare)(const void *a, const void *b))
{
	int i = nel / 2 - 1;

	while(i >= 0)
	{
		heapify(base, i, nel, width, compare);
		i--;
	}
}

void hsort(void *base, size_t nel, size_t width, 
        int (*compare)(const void *a, const void *b)) 
{ 
	build_heap(base, nel, width, compare);
	int i = nel - 1;

	while(i > 0)
	{
		swap(base, base + i * width, width);
		heapify(base, 0, i, width, compare);
		i--;
	}
}

int char_count(char *a, char target)
{
	int count = 0;
	char c = *a;
	while(c)
	{
		if(c == target) count++;

		c = *(++a);
	}
	return count;
}

int compare(const void *a, const void *b)
{
	char *ca = *((char**)a), *cb = *((char**)b);
	return char_count(ca, 'a') - char_count(cb, 'a');
}

void get_sarr(int size, char ***words_arr)
{
	char **words = NULL;

	for(int i = 0; i < size; i++)
	{
		char c;
		scanf("%c", &c);

		int wlen = 0;
		char *word = (char*)malloc(sizeof(char));
		
		while(c != '\n')
		{
			word[wlen] = c;
			word = (char*)realloc(word, sizeof(char) * (++wlen + 1));
			scanf("%c", &c);
		}
		word[wlen] = '\0';

		words = (char**)realloc(words, sizeof(char*) * (i + 1));
		words[i] = word;
	}

	*words_arr = words;
}

int main()
{
	int n;
	scanf("%i\n", &n);
	char **words;
	get_sarr(n, &words);
	for(int i = 0; i < n; i++)
		printf("%s\n", words[i]);
	hsort(words, n, sizeof(char*), compare);

	for(int i = 0; i < n; i++)
	{
		printf("%s\n", words[i]);
		free(words[i]);
	}
	free(words);

	return 0;
}