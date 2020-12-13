// разделение строки по пробелам и запись слов в массив
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

// получение массива строк размера size
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

void free_sarr(int size, char **words)
{
	for(int i = 0; i < size; i++)
		free(words[i]);
	free(words);
}

// считывает строку и сохраняет указатель на нёё в динамической памяти по адресу p
int get_string(char **p)
{
	char *string = (char*)malloc(sizeof(char)), c;
	scanf("%c", &c);
	int slen = 0, tr_slen = -1;
	while(c != '\n')
	{
		string[slen] = c;
		string = (char*)realloc(string, sizeof(char) * (++slen + 1));
		scanf("%c", &c);
	}
	string[slen] = '\0';
	*p = string;
	return slen;
}