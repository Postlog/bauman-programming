#include <iostream>
#include <vector>
#include "declaration.h"



int main() {
    Formula f("(b - c) / (c * (g - f * a))");
    for(const auto & i : f) {
        std::cout << i << std::endl;
    }
    return 0;
}
