#include <iostream>
#include "declaration.h"

void display(Queue q) {
    for(int i = 0; i < q.getCount(); i++) {
        std::cout << q[i] << ' ';
    }
    std::cout << std::endl;
}

void mutateAndDisplay(Queue queue) {
    queue.enqueue(100);
    display(queue);
}

int main() {
    Queue queue(3);
    queue.enqueue(200);
    queue.enqueue(300);
    // Объект копируется, изменяется и выводится на экран
    mutateAndDisplay(queue);
    // При этом исходный объект никак не изменяется
    display(queue);

    // Оъект копируется
    Queue queue1 = queue;
    // Изменяем исоходный объект
    queue.dequeue();
    // При этом новый объект никак не меняется
    display(queue);
    display(queue1);

    return 0;
}
