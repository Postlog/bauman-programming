#include <stdio.h>
#include <stdlib.h>

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

int sift_down(PriorityQueueUnit *list, int size)
{
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
    PriorityQueue queue;
    init_priority_queue(&queue, n);
    
    PriorityQueueUnit unit;
    unit.value = 0;
    unit.priority = 0;

    for(int i = 0; i < n; i++)
        insert(&queue, unit);

    int k;
    scanf("%d", &k);
    int max_total_time = 0;
    for(int i = 0; i < k; i++)
    {
        int start_time, working_time;
        scanf("%d %d", &start_time, &working_time);

        int total_time = pop(&queue).value;
        if (start_time > total_time) total_time = start_time;

        total_time += working_time;
        if (max_total_time < total_time) max_total_time = total_time;

        PriorityQueueUnit unit;
        unit.value = total_time;
        unit.priority = -total_time;
        insert(&queue, unit);
    }

    printf("%d\n", max_total_time);
    free(queue.heap);
}