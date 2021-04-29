#include <algorithm>
#include "declaration.h"

Queue::Queue(int capacity) : capacity(capacity) {
    this->data = new int[capacity];
    this->count = 0;
    this->head = 0;
    this->tail = 0;
}

Queue::Queue(const Queue &obj) : capacity(obj.capacity), count(obj.count), head(obj.head), tail(obj.tail) {
    this->data = new int[this->capacity];
    std::copy(obj.data, obj.data + obj.capacity, this->data);
}

Queue::~Queue() {
    delete[] this->data;
}

bool Queue::isEmpty() {
    return this->count == 0;
}

void Queue::enqueue(int value) {
    this->data[this->tail] = value;
    this->tail++;
    if (this->tail == this->capacity) {
        this->tail = 0;
    }
    this->count++;
}

int Queue::dequeue() {
    int value = this->data[this->head];
    this->head++;
    if (this->head == this->capacity) {
        this->head = 0;
    }
    this->count--;
    return value;
}

int &Queue::operator[](int index) {
    return this->data[this->head + index];
}

Queue &Queue::operator=(const Queue &obj) {
    if (this != &obj) {
        this->capacity = obj.capacity;
        this->tail = obj.tail;
        this->head = obj.head;
        this->count = obj.count;

        delete [] this->data;
        this->data = new int[obj.capacity];
        std::copy(obj.data, obj.data + obj.capacity, this->data);
    }

    return *this;
}

int Queue::getCount() {
    return this->count;
}

int Queue::getCapacity() {
    return this->capacity;
}