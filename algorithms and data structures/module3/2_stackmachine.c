#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Stack
{
	int capacity;
	int top;
	int *data;
};

void stack_init(struct Stack *s, int n)
{
	s->data = (int *)malloc(sizeof(int) * n);
	s->capacity = n;
	s->top = 0;
}

void stack_push(struct Stack *s, int x)
{
	s->data[s->top] = x;
	s->top++;
}

int stack_pop(struct Stack *s)
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
	stack_init(&stack, n);

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
				stack_push(&stack, x);
				break;
			case 1:
				a = stack_pop(&stack);
				b = stack_pop(&stack);
				stack_push(&stack, a + b);
				break;
			case 2:
				a = stack_pop(&stack);
				b = stack_pop(&stack);
				stack_push(&stack, a - b);
				break;
			case 3:
				a = stack_pop(&stack);
				b = stack_pop(&stack);
				stack_push(&stack, a * b);
				break;
			case 4:
				a = stack_pop(&stack);
				b = stack_pop(&stack);
				stack_push(&stack, a / b);
				break;
			case 5:
				a = stack_pop(&stack);
				b = stack_pop(&stack);
				if(a > b)
					stack_push(&stack, a);
				else
					stack_push(&stack, b);
				break;
			case 6:
				a = stack_pop(&stack),
				b = stack_pop(&stack);
				if(a < b)
					stack_push(&stack, a);
				else
					stack_push(&stack, b);
				break;
			case 7:
				a = stack_pop(&stack);
				
				stack_push(&stack, -a);
				break;
			case 8:
				a = stack_pop(&stack);
				
				stack_push(&stack, a);
				stack_push(&stack, a);
				break;
			case 9:
				a = stack_pop(&stack);
				b = stack_pop(&stack);
				
				stack_push(&stack, a);
				stack_push(&stack, b);
				break;
		}
	}
	printf("%i\n", stack_pop(&stack));
	free(stack.data);
}
