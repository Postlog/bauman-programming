#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void substr(char *s, char *dest_s, int start, int end) // [start; end)
{
	for(int i = start; i < end; i++)
		dest_s[i - start] = s[i];
	dest_s[end - start] = '\0';
}

int overlap(char *s1, char *s2)
{
	int s1_len = strlen(s1),
		s2_len = strlen(s2),
		overlap_ = 0;

	for(int i = s1_len - 1, j = 1; i > 0 && j < s2_len; i--, j++)
	{
		char s1_suff[s1_len - i + 1],
			 s2_pref[j + 1];
		substr(s1, s1_suff, i, s1_len);
		substr(s2, s2_pref, 0, j);
		if(strcmp(s1_suff, s2_pref) == 0) overlap_ = j;
	} 

	return overlap_;
}

int merge(char *s1, char *s2, char **p_dest, int len)
{
	int s1_len = strlen(s1),
		s2_len = strlen(s2);
	char s2_subs[s2_len - len + 1];
	substr(s2, s2_subs, len, s2_len);
	char *merged = (char *)malloc(sizeof(int) * (s1_len + strlen(s2_subs)));
	int i = 0;
	while(*s1)
		merged[i++] = *(s1++);

	char *p_s2_subs = s2_subs;
	while(*p_s2_subs)
		merged[i++] = *(p_s2_subs++);
	merged[i] = '\0';

	*p_dest = merged;
	return i;
}

int superstring_length(char **strings, int n)
{
	int strings_size = n;

	while(strings_size > 1)
	{
		int max_overlap = 0;
		char *msi = NULL, *msj = NULL;

		int msi_idx, msj_idx;

		for(int i = 0; i < n; i++)
		{
			if(strings[i] != NULL)
			{
				if(msi == NULL)
				{
					msi = strings[i];
					msi_idx = i;
				}
				else
				{
					msj = strings[i];
					msj_idx = i;
					break;
				}
			}
		}

		for(int i = 0; i < n; i++)
		{
			if(strings[i] == NULL) continue;

			for(int j = 0; j < n; j++)
			{
				if(strings[j] == NULL) continue;

				if(strcmp(strings[i], strings[j]) == 0) continue;
				int current_overlap = overlap(strings[i], strings[j]);

				if(current_overlap > max_overlap)
				{
					max_overlap = current_overlap;
					msi = strings[i];
					msj = strings[j];
					msi_idx = i;
					msj_idx = j;
				}
			}
		}

		
		char *merged;
		merge(msi, msj, &merged, max_overlap);
		free(strings[msi_idx]);
		free(strings[msj_idx]);
		strings[msi_idx] = NULL;
		strings[msj_idx] = merged;
		strings_size -= 1;
	}

	for(int i = 0; i < n; i++)
	{
		if(strings[i] != NULL)
		{
			int len = strlen(strings[i]);
			free(strings[i]);
			return len;
		}
	}

	return 0; // never be reached
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
int main(int argc, char** argv)
{
	int n;
	scanf("%i ", &n);
	char **words;
	get_sarr(n, &words);
	printf("%i\n", superstring_length(words, n));
	free(words);
	return 0;
}