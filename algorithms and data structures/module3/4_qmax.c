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

void double_stack_init(struct DoubleStack *ds, int n)
{
	ds->capacity = n;
	ds->top_1 = 0;
	ds->top_2 = n - 1;
	ds->data = (int *)malloc(sizeof(int) * n);
}

void double_stack_push_1(struct DoubleStack *ds, int x)
{
	ds->data[ds->top_1] = x;
	ds->top_1++;
}

void double_stack_push_2(struct DoubleStack *ds, int x)
{
	ds->data[ds->top_2] = x;
	ds->top_2--;
}

int double_stack_pop_1(struct DoubleStack *ds)
{
	ds->top_1--;
	return ds->data[ds->top_1];
}

int double_stack_pop_2(struct DoubleStack *ds)
{
	ds->top_2++;
	return ds->data[ds->top_2];
}

int double_stack_empty_1(struct DoubleStack ds)
{
	return ds.top_1 == 0;
}

int double_stack_empty_2(struct DoubleStack ds)
{
	return ds.top_2 == ds.capacity - 1;
}

void double_stack_clear(struct DoubleStack *ds)
{
	free(ds->data);
	ds->data = NULL;
	ds->capacity = 0;
	ds->top_1 = -1;
	ds->top_2 = -1;
}

void queue_on_stack_init(struct DoubleStack *ds, int n)
{
	double_stack_init(ds, n);
}

void enqueue(struct DoubleStack *ds, int x, int *max)
{
	if(x > *max) *max = x;
	double_stack_push_1(ds, x);
}

void queue_on_stack_recalculate_max(struct DoubleStack *ds, int *max)
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
	if(double_stack_empty_2(*ds) == 1)
	{
		while(double_stack_empty_1(*ds) != 1)
		{
			int a = double_stack_pop_1(ds);
			if(a > *max) *max = a;
			double_stack_push_2(ds, a);
		}
	}
	int dequeued = double_stack_pop_2(ds);
	if(dequeued == *max)
		queue_on_stack_recalculate_max(ds, max);

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
	double_stack_init(&stack, 2 * n);
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
				printf("%s\n", double_stack_empty_1(stack) && double_stack_empty_2(stack) ? "true" : "false");
				break;
		}

	}
	double_stack_clear(&stack);
}