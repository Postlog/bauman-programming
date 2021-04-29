#ifndef LAB8_OPERATION_CPP
#define LAB8_OPERATION_CPP

template<char O, class L, class R>
class Operation {
private:
    L left;
    R right;
public:
    Operation(L left, R right): left(left), right(right) {}
    int compute();
};

template<char O, char O1, char O2, class L1, class L2, class R1, class R2>
class Operation<O, Operation<O1, L1, R1>, Operation<O2, L2, R2>> {
private:
    Operation<O1, L1, R1> left;
    Operation<O2, L2, R2> right;
public:
    Operation(Operation<O1, L1, R1> left, Operation<O2, L2, R2> right) : left(left), right(right) {}
    int compute();
};

template<char O, char O1, class L1, class R1>
class Operation<O, Operation<O1, L1, R1>, int> {
private:
    Operation<O1, L1, R1> left;
    int right;
public:
    Operation(Operation<O1, L1, R1> left, int right) : left(left), right(right) {}
    int compute();
};

template<char O, char O1, class L1, class R1>
class Operation<O, int, Operation<O1, L1, R1>> {
private:
    int left;
    Operation<O1, L1, R1> right;
public:
    Operation(int left, Operation<O1, L1, R1> right) : left(left), right(right) {}
    int compute();
};

template<char O, char O1, class L1, class R1>
class Operation<O, int&, Operation<O1, L1, R1>> {
private:
    int &left;
    Operation<O1, L1, R1> right;
public:
    Operation(int &left, Operation<O1, L1, R1> right) : left(left), right(right) {}
    int compute();
};

template<char O, class L, class R>
int Operation<O, L, R>::compute() {
    if (O == '+') return this->left + this->right;
    else if (O == '*') return this->left * this->right;
    else if (O == '=') return this->left = this->right;

    throw std::invalid_argument("illegal operation");
}

template<char O, char O1, char O2, class L1, class L2, class R1, class R2>
int Operation<O, Operation<O1, L1, R1>, Operation<O2, L2, R2>>::compute() {
    if (O == '+') return this->left.compute() + this->right.compute();
    else if (O == '*') return this->left.compute() * this->right.compute();

    throw std::invalid_argument("illegal operation");
}

template<char O, char O1, class L1, class R1>
int Operation<O, Operation<O1, L1, R1>, int>::compute() {
    if (O == '+') return this->left.compute() + this->right;
    else if (O == '*') return this->left.compute() * this->right;

    throw std::invalid_argument("illegal operation");
}

template<char O, char O1, class L1, class R1>
int Operation<O, int, Operation<O1, L1, R1>>::compute() {
    if (O == '+') return this->left + this->right.compute();
    else if (O == '*') return this->left * this->right.compute();

    throw std::invalid_argument("illegal operation");
}

template<char O, char O1, class L1, class R1>
int Operation<O, int &, Operation<O1, L1, R1>>::compute() {
    if (O == '+') return this->left + this->right.compute();
    else if (O == '*') return this->left * this->right.compute();
    else if (O == '=') return this->left = this->right.compute();

    throw std::invalid_argument("illegal operation");
}

#endif
