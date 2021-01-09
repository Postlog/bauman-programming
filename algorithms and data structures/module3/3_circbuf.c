#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct CircleBuffer
{
	int *data;
	int capacity;
	int count;
	int head;
	int tail;
};

void init_queue(struct CircleBuffer *cb, int n)
{
	cb->data = (int *)malloc(sizeof(int) * n);
	cb->capacity = n;
	cb->count = 0;
	cb->head = 0;
	cb->tail = 0;
}

int queue_empty(struct CircleBuffer cb)
{
	return cb.count == 0;
}

void enqueue(struct CircleBuffer *cb, int x)
{
	cb->data[cb->tail] = x;
	cb->tail++;
	// if(cb->tail == cb->capacity) cb->tail = 0;
	if(cb->tail == cb->capacity)
	{
		cb->capacity *= 2;
		cb->data = (int *)realloc(cb->data, sizeof(int) * cb->capacity);
	}
	cb->count++;
}

int dequeue(struct CircleBuffer *cb)
{
	int x = cb->data[cb->head];
	cb->head++;
	if(cb->head == cb->capacity) cb->head = 0;
	cb->count--;

	return x;
}

int ACTIONS_COUNT = 3;
char *ACTIONS[] = {"ENQ", "DEQ", "EMPTY"};

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
	struct CircleBuffer buffer;
	init_queue(&buffer, 4);

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
				enqueue(&buffer, x);
				break;
			case 1:
				printf("%d\n", dequeue(&buffer));
				break;
			case 2:
				printf("%s\n", queue_empty(buffer) ? "true" : "false");
				break;
		}
	}
	free(buffer.data);
}