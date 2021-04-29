#ifndef LAB7_DECLARATION_H
#define LAB7_DECLARATION_H

class Queue {
private:
    int *data, capacity, count, head, tail;
public:
    Queue(int);
    Queue(const Queue&);

    bool isEmpty();
    int getCount();
    int getCapacity();
    void enqueue(int);
    int dequeue();

    int& operator[](int);
    Queue& operator=(const Queue&);

    virtual ~Queue();
};

#endif
