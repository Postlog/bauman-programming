#include <iostream>
#include "operation.cpp"



int main() {
    Operation<'+',int, int> operation(10, 20);
    std::cout << operation.compute() << std::endl;
    return 0;
}
