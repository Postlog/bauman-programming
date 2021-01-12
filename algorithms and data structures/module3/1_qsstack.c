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

void stack_init(struct Stack *s, int n)
{
	s->data = (struct Task *)malloc(sizeof(struct Task) * n);
	s->capacity = n;
	s->top = 0;
}

void stack_push(struct Stack *s, struct Task x)
{
	s->data[s->top] = x;
	s->top++;
}

struct Task stack_pop(struct Stack *s)
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
	stack_init(&tasks, n);
	struct Task init_task;
	init_task.low = 0;
	init_task.high = n - 1;
	stack_push(&tasks, init_task);

	while(!stack_empty(tasks))
	{
		struct Task task = stack_pop(&tasks);
		int low = task.low,
			high = task.high;
		if(low < high)
		{
			struct Task new_task;
			int q = partition(a, low, high);
			new_task.low = q + 1;
			new_task.high = high;
			stack_push(&tasks, new_task);

			new_task.low = low;
			new_task.high = q - 1;
			stack_push(&tasks, new_task);
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