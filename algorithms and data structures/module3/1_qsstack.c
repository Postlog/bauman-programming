#include <stdio.h>
#include <stdlib.h>

struct Task
{
	int low;
	int high;
};

struct Stack
{
	int capacity;
	int top;
	struct Task *data;
};

void init_stack(struct Stack *s, int n)
{
	s->data = (struct Task *)malloc(sizeof(struct Task) * n);
	s->capacity = n;
	s->top = 0;
}

void push(struct Stack *s, struct Task x)
{
	s->data[s->top] = x;
	s->top++;
}

struct Task pop(struct Stack *s)
{
	s->top--;
	return s->data[s->top];
}

int stack_empty(struct Stack s)
{
	return s.top == 0;
}

int partition(int *a, int low, int high)
{
	int i = low, j = low;

	for(; j < high; j++)
	{
		if(a[j] < a[high])
		{
			int temp = a[i];
			a[i] = a[j];
			a[j] = temp;

			i++;
		}
	}

	int temp = a[i];
	a[i] = a[high];
	a[high] = temp;

	return i;
}

void quicksort_stack(int *a, int n)
{
	struct Stack tasks;
	init_stack(&tasks, n);
	struct Task init_task;
	init_task.low = 0;
	init_task.high = n - 1;
	push(&tasks, init_task);

	while(!stack_empty(tasks))
	{
		struct Task task = pop(&tasks);
		int low = task.low,
			high = task.high;
		if(low < high)
		{
			struct Task new_task;
			int q = partition(a, low, high);
			new_task.low = q + 1;
			new_task.high = high;
			push(&tasks, new_task);

			new_task.low = low;
			new_task.high = q - 1;
			push(&tasks, new_task);
		}
	}
	free(tasks.data);
}

void main()
{
	int n;
	scanf("%d", &n);
	int nums[n];
	for(int i = 0; i < n; i++) scanf("%d", &nums[i]);
	quicksort_stack(nums, n);
	for(int i = 0; i < n; i++) printf("%d ", nums[i]);
	printf("\n");
}