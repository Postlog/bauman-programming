#include <iostream>
#include <cmath>
// номер 65
template <typename T>
class Curve {
private:
    T a;
public:
    Curve(T a) {
        this->a = a;
    }

    Curve<T> operator+(const Curve<T> &another) const {
        return Curve(this->a + another.a);
    }

    Curve<T> operator-(const Curve<T> &another) const {
        return Curve(this->a - another.a);
    }

    Curve<T> operator*(T multiplier) const {
        return Curve(this->a * multiplier);
    }


    T operator()(T from, T to) const {
        return this->a * (log(to) - log(from));
    }
};

template<typename T>
Curve<T> operator*(T multiplier, const Curve<T> &curve) {
    return curve * multiplier;
}

int main() {
    Curve<double> c(1);
    auto c1 = c + Curve<double>(3);
    std::cout << c(10, 20) << std::endl;
    std::cout << c1(10, 20) << std::endl;
    return 0;
}
