#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int INT_MIN = -2147483647 - 1;

struct DoubleStack
{
	int capacity;
	int top_1;
	int top_2;
	int *data;
};

void init_double_stack(struct DoubleStack *ds, int n)
{
	ds->capacity = n;
	ds->top_1 = 0;
	ds->top_2 = n - 1;
	ds->data = (int *)malloc(sizeof(int) * n);
}

void push_1(struct DoubleStack *ds, int x)
{
	ds->data[ds->top_1] = x;
	ds->top_1++;
}

void push_2(struct DoubleStack *ds, int x)
{
	ds->data[ds->top_2] = x;
	ds->top_2--;
}

int pop_1(struct DoubleStack *ds)
{
	ds->top_1--;
	return ds->data[ds->top_1];
}

int pop_2(struct DoubleStack *ds)
{
	ds->top_2++;
	return ds->data[ds->top_2];
}

int empty_1(struct DoubleStack ds)
{
	return ds.top_1 == 0;
}

int empty_2(struct DoubleStack ds)
{
	return ds.top_2 == ds.capacity - 1;
}

void clear_double_stack(struct DoubleStack *ds)
{
	free(ds->data);
	ds->data = NULL;
	ds->capacity = 0;
	ds->top_1 = -1;
	ds->top_2 = -1;
}

void init_queue_on_stack(struct DoubleStack *ds, int n)
{
	init_double_stack(ds, n);
}

void enqueue(struct DoubleStack *ds, int x, int *max)
{
	if(x > *max) *max = x;
	push_1(ds, x);
}

void recalculate_max(struct DoubleStack *ds, int *max)
{
	*max = INT_MIN;
		
	for(int i = ds->top_2 + 1; i < ds->capacity; i++)
		if(*max < ds->data[i])
			*max = ds->data[i];
			
	for(int i = 0; i < ds->top_1; i++)
		if(*max < ds->data[i])
			*max = ds->data[i];
}

int dequeue(struct DoubleStack *ds, int *max)
{
	if(empty_2(*ds) == 1)
	{
		while(empty_1(*ds) != 1)
		{
			int a = pop_1(ds);
			if(a > *max) *max = a;
			push_2(ds, a);
		}
	}
	int dequeued = pop_2(ds);
	if(dequeued == *max)
		recalculate_max(ds, max);

	return dequeued;
}

int ACTIONS_COUNT = 4;
char *ACTIONS[] = {"ENQ", "DEQ", "MAX", "EMPTY"};

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
	struct DoubleStack stack;
	init_double_stack(&stack, 2 * n);
	int max = INT_MIN;
	for(int i = 0; i < n; i++)
	{
		char action[6];
		scanf("%s", action);
		int index = action_index(action);
		int x;
		switch(index)
		{
			case 0:
				scanf("%d", &x);
				enqueue(&stack, x, &max);
				break;
			case 1:
				printf("%d\n", dequeue(&stack, &max));
				break;
			case 2:
				printf("%d\n", max);
				break;
			case 3:
				printf("%s\n", empty_1(stack) && empty_2(stack) ? "true" : "false");
				break;
		}

	}
	clear_double_stack(&stack);
}