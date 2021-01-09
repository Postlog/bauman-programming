#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Stack
{
	int capacity;
	int top;
	int *data;
};

void init_stack(struct Stack *s, int n)
{
	s->data = (int *)malloc(sizeof(int) * n);
	s->capacity = n;
	s->top = 0;
}

void push(struct Stack *s, int x)
{
	s->data[s->top] = x;
	s->top++;
}

int pop(struct Stack *s)
{
	s->top--;
	return s->data[s->top];
}

int ACTIONS_COUNT = 10;
char *ACTIONS[] = {"CONST", "ADD", "SUB", "MUL", "DIV", "MAX", "MIN", "NEG", "DUP", "SWAP"};

int action_index(char *action)
{
	for(int i = 0; i < ACTIONS_COUNT; i++)
		if(strcmp(action, ACTIONS[i]) == 0) return i;
	return -1;
}

void main()
{
	int n;
	scanf("%d", &n);
	struct Stack stack;
	init_stack(&stack, n);

	for(int i = 0; i < n; i++)
	{
		char action[6];
		scanf("%s", action);

		int index = action_index(action);
		int a, b, x;

		switch(index)
		{
			case 0:
				scanf("%d", &x);
				push(&stack, x);
				break;
			case 1:
				a = pop(&stack);
				b = pop(&stack);
				push(&stack, a + b);
				break;
			case 2:
				a = pop(&stack);
				b = pop(&stack);
				push(&stack, a - b);
				break;
			case 3:
				a = pop(&stack);
				b = pop(&stack);
				push(&stack, a * b);
				break;
			case 4:
				a = pop(&stack);
				b = pop(&stack);
				push(&stack, a / b);
				break;
			case 5:
				a = pop(&stack);
				b = pop(&stack);
				if(a > b)
					push(&stack, a);
				else
					push(&stack, b);
				break;
			case 6:
				a = pop(&stack),
				b = pop(&stack);
				if(a < b)
					push(&stack, a);
				else
					push(&stack, b);
				break;
			case 7:
				a = pop(&stack);
				
				push(&stack, -a);
				break;
			case 8:
				a = pop(&stack);
				
				push(&stack, a);
				push(&stack, a);
				break;
			case 9:
				a = pop(&stack);
				b = pop(&stack);
				
				push(&stack, a);
				push(&stack, b);
				break;
		}
	}
	printf("%i\n", pop(&stack));
	free(stack.data);
}
