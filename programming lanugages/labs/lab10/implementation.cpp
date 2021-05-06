#include <iostream>
#include "declaration.h"

std::vector<Lexem> Tokenizer::tokenize() {
    std::vector<Lexem> lexems;
    for (int i = 0; i < this->expression.size(); i++) {
        switch (this->expression.at(i)) {
            case ' ':
            case '\n':
            case '\t':
                continue;
            case '+':
                lexems.push_back(Lexem{Plus, "+"});
                break;
            case '-':
                lexems.push_back(Lexem{Minus, "-"});
                break;
            case '/':
                lexems.push_back(Lexem{Div, "/"});
                break;
            case '*':
                lexems.push_back(Lexem{Multiply, "*"});
                break;
            case '(':
                lexems.push_back(Lexem{OpenBracket, "("});
                break;
            case ')':
                lexems.push_back(Lexem{CloseBracket, ")"});
                break;
            default:
                if (isalpha(this->expression[i])) {
                    std::string variable_image;
                    while (i < this->expression.size() &&
                           (isalpha(this->expression[i]) || isdigit(this->expression[i]))) {
                        variable_image += this->expression[i++];
                    }
                    i--;
                    lexems.push_back(Lexem{Variable, variable_image});
                } else {
                    throw std::exception();
                }
        }
    }
    return lexems;
}

Tokenizer::Tokenizer(std::string expression) : expression(expression) {}

Tokenizer::Stream Tokenizer::get_stream() {
    return Stream(this->tokenize());
}

Lexem Tokenizer::Stream::watch_next() {
    return lexems[index];
}

Lexem Tokenizer::Stream::next() {
    if (index >= lexems.size()) {
        throw std::exception();
    }
    return lexems[index++];
}

bool Tokenizer::Stream::has_next() {
    return index < lexems.size();
}

Tokenizer::Stream::Stream(std::vector<Lexem> lexems) : lexems(lexems), index(0) {}

void Formula::parse_operations() {
    parse_expression();
}

void Formula::parse_expression() {
    parse_term();
    if (stream.has_next() && (stream.watch_next().tag & (Plus | Minus))) {
        auto image = stream.next().image;
        parse_expression();
        operations.push_back(image);
    }
}

void Formula::parse_term() {
    parse_factor();
    if (stream.has_next() && (stream.watch_next().tag & (Multiply | Div))) {
        auto image = stream.next().image;
        parse_term();
        operations.push_back(image);
    }
}

void Formula::parse_factor() {
    auto lexem = stream.next();
    switch (lexem.tag) {
        case Variable:
            return;
        case OpenBracket:
            parse_expression();
            if (stream.next().tag != CloseBracket) {
                throw std::exception();
            }
            return;
        case Minus:
            operations.push_back("-");
            parse_factor();
            break;
        default:
            throw std::exception();
    }
}

Formula::Formula(std::string formula) : stream(Tokenizer(std::move(formula)).get_stream()) {
    this->parse_operations();
}

Formula::Iterator Formula::begin() {
    return this->operations.data();
}

Formula::Iterator Formula::end() {
    return this->operations.data() + this->operations.size();
}

Formula::Iterator::Iterator(std::string *current) : current(current) {}

Formula::Iterator& Formula::Iterator::operator++() {
    this->current++;
    return *this;
}

Formula::Iterator Formula::Iterator::operator++(int) {
    Formula::Iterator temp = *this;
    this->current++;
    return temp;
}

bool Formula::Iterator::operator!=(const Iterator &it) {
    return this->current != it.current;
}

const std::string &Formula::Iterator::operator*() {
    return *current;
}