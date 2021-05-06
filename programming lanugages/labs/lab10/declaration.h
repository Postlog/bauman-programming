
#ifndef LAB10_DECLARATION_H
#define LAB10_DECLARATION_H

#include <string>
#include <vector>

enum Tag {
    Variable = 0,
    Plus = 1, Minus = 2, Multiply = 4, Div = 8,
    OpenBracket = 16, CloseBracket = 32
};

struct Lexem {
    Tag tag;
    std::string image;
};

class Tokenizer {
private:
    std::string expression;
    std::vector<Lexem> tokenize();
public:
    class Stream;
    Tokenizer(std::string expression);
    Stream get_stream();

    class Stream {
    private:
        std::vector<Lexem> lexems;
        int index;
    public:
        Lexem watch_next();
        Lexem next();
        bool has_next();
        Stream(std::vector<Lexem> lexems);
    };
};

class Formula {
private:
    std::vector<std::string> operations;
    Tokenizer::Stream stream;

    void parse_operations();
    void parse_expression();
    void parse_term();
    void parse_factor();

public:
    class Iterator;
    explicit Formula(std::string formula);

    Iterator begin();
    Iterator end();

    class Iterator {
    private:
        std::string *current;
    public:
        Iterator(std::string* current);

        Iterator& operator++();
        Iterator operator++(int);
        bool operator!=(const Iterator& it);
        const std::string& operator*();
    };
};


#endif //LAB10_DECLARATION_H
