#include <stdio.h>
#include <stdlib.h>

typedef struct PriorityQueueUnit PriorityQueueUnit;
typedef struct PriorityQueue PriorityQueue;
typedef struct Task Task;

int INT_MIN = -2147483647 - 1;

struct Task
{
    int start_time, duration;
};

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

int priority_queue_sift_down(PriorityQueueUnit *list, int size)
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

void priority_queue_init(PriorityQueue *q, int capacity)
{
    q->capacity = capacity;
    q->count = 0;
    q->heap = (PriorityQueueUnit *)malloc(sizeof(PriorityQueueUnit) * capacity);
}

PriorityQueueUnit priority_queue_pop(PriorityQueue *q)
{
    PriorityQueueUnit x = q->heap[0];
    q->heap[0].priority = INT_MIN;
    q->count = priority_queue_sift_down(q->heap, q->capacity);
    return x;
}

void priority_queue_insert(PriorityQueue *q, PriorityQueueUnit value)
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
    priority_queue_init(&queue, n);
    
    PriorityQueueUnit unit;
    unit.value = 0;
    unit.priority = 0;

    for(int i = 0; i < n; i++)
        priority_queue_insert(&queue, unit);

    int k;
    scanf("%d", &k);

    Task tasks[k];
    for(int i = 0; i < k; i++)
    {
        int start_time, duration;
        scanf("%d %d", &start_time, &duration);
        tasks[i].start_time = start_time;
        tasks[i].duration = duration;
    }
    

    int max_total_time = 0;
    for(int i = 0; i < k; i++)
    {
        
        Task task = tasks[i];
        int total_time = priority_queue_pop(&queue).value;
        if (task.start_time > total_time)
            total_time = task.start_time;

        total_time += task.duration;
        if (max_total_time < total_time) max_total_time = total_time;

        PriorityQueueUnit unit;
        unit.value = total_time;
        unit.priority = -total_time;
        priority_queue_insert(&queue, unit);
    }

    printf("%d\n", max_total_time);
    free(queue.heap);
}