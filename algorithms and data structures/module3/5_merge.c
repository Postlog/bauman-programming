#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct PriorityQueueUnit PriorityQueueUnit;
typedef struct PriorityQueue PriorityQueue;

int INT_MIN = -2147483647 - 1;

struct PriorityQueueUnit
{
    int value;
    int priority;
};

struct PriorityQueue {
    PriorityQueueUnit *heap;
    int capacity;
    int count;
};


void swap(PriorityQueueUnit *a, PriorityQueueUnit *b)
{
    PriorityQueueUnit t = *a;
    *a = *b;
    *b = t;
}

int sift_down(PriorityQueueUnit *list, int size) {
    int capacity = 0, i = 0;
    while (i < size / 2)
    {
        int left = i * 2 + 1,
            right = left + 1,
            j = left;

        if(right < size && list[right].priority > list[left].priority) 
            j = right;

        if(list[i].priority >= list[j].priority)
            return capacity;

        swap(&list[i], &list[j]);
        i = j;
        capacity = i;
    }
    return capacity;
}

void init_priority_queue(PriorityQueue *q, int capacity)
{
    q->capacity = capacity;
    q->count = 0;
    q->heap = (PriorityQueueUnit *)malloc(sizeof(PriorityQueueUnit) * capacity);
}

PriorityQueueUnit pop(PriorityQueue *q)
{
    PriorityQueueUnit x = q->heap[0];
    q->heap[0].priority = INT_MIN;
    q->count = sift_down(q->heap, q->capacity);
    return x;
}

void insert(PriorityQueue *q, PriorityQueueUnit value)
{
    int i = q->count;
    q->heap[i] = value;
    while (i > 0 && q->heap[i].priority > q->heap[(i - 1) / 2].priority)
    {

        swap(&q->heap[i], &q->heap[(i - 1) / 2]);
        i = (i - 1) / 2;
    }
    q->count++;
}

int main() {
    int n;
    scanf("%d", &n);
    int total_size = 0;
    for(int i = 0; i < n; i++)
    {
        int size;
        scanf("%d", &size);
        total_size += size;
    }
    PriorityQueue queue;
    init_priority_queue(&queue, total_size);

    for(int i = 0; i < total_size; i++)
    {
        int val;
        scanf("%d", &val);

        PriorityQueueUnit unit;
        unit.value = val;
        unit.priority = -val;

        insert(&queue, unit);
    }

    for(int i = 0; i < total_size; i++)
        printf("%d ", pop(&queue).value);
    printf("\n");
    free(queue.heap);
}