#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct Elem Elem;

struct Elem
{
	Elem *next;
    char *word;
};

int compare(Elem *a, Elem *b)
{
	return strlen(a->word) > strlen(b->word);
}

void swap(Elem *head)
{
	char *temp = head->word;
	head->word = head->next->word;
	head->next->word = temp;
} 

int length(Elem *head) 
{
	int len = 0;
	for(Elem *el = head; el != NULL; el = el->next, len++);
	return len;
}

void bsort(Elem *head,  int (*compare)(Elem *a, Elem *b))
{
	Elem *el;
	int i = length(head);
	while(--i)
	{
		int len_ = i;
		el = head;

		for(i = 0; i < len_; i++, el = el->next)
			if(compare(el, el->next)) swap(el);
	}
}
void main()
{
	Elem *head = (Elem *)malloc(sizeof(Elem)), *tail;
	head->next = NULL;

	char *word;
	char str[100000];
	gets(str);
	
	word = strtok(str, " ");
	head->word = word;
	word = strtok(NULL," ");
	while(word)
	{
		Elem *el = (Elem *)malloc(sizeof(Elem));
		el->word = word;

		if (!head->next) 
			head->next = el;
		else
			tail->next = el;
		tail = el;			  
		word = strtok(NULL," ");
		if(!word) tail->next = NULL;
	}
	bsort(head, compare);
	Elem *el = head;

	while(el)
	{
		printf("%s ", el->word);
		Elem *prev = el;
		el = el->next;		
		free(prev);	
	}
	printf("\n");
}