package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

// UTILS

func isDigit(char rune) bool {
	return char >= '0' && char <= '9'
}

func isAlphabetic(char rune) bool {
	return char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z'
}

// COMMON TYPES
// -- SET

type Set struct {
	Storage map[string]bool
}

func NewSet() *Set {
	return &Set{map[string]bool{}}
}

func (set *Set) Contains(value string) bool {
	_, ok := set.Storage[value]
	return ok
}

func (set *Set) ContainsAll(another *Set) bool {
	for key := range another.Storage {
		if !set.Contains(key) {
			return false
		}
	}
	return true
}

func (set *Set) Add(value string) {
	set.Storage[value] = true
}

func (set *Set) Size() int {
	return len(set.Storage)
}

func (set *Set) AsSlice() []string {
	values := make([]string, len(set.Storage))
	i := 0
	for key := range set.Storage {
		values[i] = key
		i++
	}
	return values
}

// -------------------------------------------

type Tag int

const (
	MINUS Tag = 1 << iota
	PLUS
	MULT
	DIV
	OPENBRACKET
	CLOSEBRACKET
	VARIABLE
	NUMBER
	EQUAL
	NOTEQUAL
	LESS
	GREATER
	LESSEQUAL
	GREATEREQUAL
	QUESTION
	COLON
	COLONEQUAL
	COMMA
	SEMICOLON
)

type Lexem struct {
	Tag   Tag
	Image string
}

type LexemStream struct {
	Lexems     []Lexem
	LexemIndex int
}

func NewStream(lexems []Lexem) *LexemStream {
	return &LexemStream{lexems, 0}
}

func (stream LexemStream) Watch() Lexem {
	return stream.Lexems[stream.LexemIndex]
}

func (stream LexemStream) WatchPrevious() Lexem {
	return stream.Lexems[stream.LexemIndex-1]
}

func (stream LexemStream) HasNext() bool {
	return stream.LexemIndex < len(stream.Lexems)
}

func (stream *LexemStream) Get() Lexem {
	if !stream.HasNext() {
		panic("Unexpected end of stream")
	}

	lexem := stream.Lexems[stream.LexemIndex]
	stream.LexemIndex++
	return lexem
}

// TOKENIZER

func Tokenize(expression_ string) (stream *LexemStream, result bool) {
	defer func() {
		if x := recover(); x != nil {
			stream = nil
			result = false
		}
	}()

	expression := []rune(expression_)
	var lexems []Lexem
	for i := 0; i < len(expression); i++ {
		var lexem Lexem
		switch expression[i] {
		case ' ', '\n', '\t':
			continue
		case ';':
			lexem = Lexem{SEMICOLON, ";"}
		case '-':
			lexem = Lexem{MINUS, "-"}

		case '+':
			lexem = Lexem{PLUS, "+"}

		case '*':
			lexem = Lexem{MULT, "*"}

		case '/':
			lexem = Lexem{DIV, "/"}

		case '(':
			lexem = Lexem{OPENBRACKET, "("}

		case ')':
			lexem = Lexem{CLOSEBRACKET, ")"}

		case '=':
			lexem = Lexem{EQUAL, "="}

		case ',':
			lexem = Lexem{COMMA, ","}

		case '>':
			if i < len(expression)-1 && expression[i+1] == '=' {
				lexem = Lexem{GREATEREQUAL, ">="}
				i++
			} else {
				lexem = Lexem{GREATER, ">"}
			}
		case '<':
			if i < len(expression)-1 {
				switch expression[i+1] {
				case '=':
					lexem = Lexem{LESSEQUAL, "<="}
					i++
				case '>':
					lexem = Lexem{NOTEQUAL, "<>"}
					i++
				default:
					lexem = Lexem{LESS, "<"}
				}
			} else {
				lexem = Lexem{LESS, "<"}
			}

		case ':':
			if i < len(expression)-1 && expression[i+1] == '=' {
				lexem = Lexem{COLONEQUAL, ":="}
				i++
			} else {
				lexem = Lexem{COLON, ":"}
			}
		case '?':
			lexem = Lexem{QUESTION, "?"}
		default:
			if isDigit(expression[i]) {
				var numberImage string
				for ; i < len(expression) && isDigit(expression[i]); i++ {
					numberImage += string(expression[i])
				}
				i--
				lexem = Lexem{NUMBER, numberImage}
			} else if isAlphabetic(expression[i]) {
				var variableNameImage string
				for ; i < len(expression) && (isAlphabetic(expression[i]) || isDigit(expression[i])); i++ {
					variableNameImage += string(expression[i])
				}
				i--
				lexem = Lexem{VARIABLE, variableNameImage}
			} else {
				panic("Unexpected character '" + string(expression[i]) + "' in expression")
			}
		}
		lexems = append(lexems, lexem)
	}
	return NewStream(lexems), true
}

// LEXER

type CalledFunctionsInfo struct {
	FunctionsNames *Set
	LineIndex      int
}

type Lexer struct {
	DefinedFunctionsNames, CalledFunctionsNames *Set
	FunctionArgumentsNames                      map[string]*Set
	Dependencies                                map[string]CalledFunctionsInfo
	FunctionCallArgumentsCounters               map[string][]int
	Stream                                      *LexemStream

	CurrentFunctionName string
	LineIndex           int
}

func NewLexer(stream *LexemStream) *Lexer {
	return &Lexer{
		Stream:                        stream,
		FunctionArgumentsNames:        map[string]*Set{},
		Dependencies:                  map[string]CalledFunctionsInfo{},
		FunctionCallArgumentsCounters: map[string][]int{},
		DefinedFunctionsNames:         NewSet(),
		CalledFunctionsNames:          NewSet(),
	}
}

func (lexer *Lexer) Parse() (result bool) {
	defer func() {
		if x := recover(); x != nil {
			result = false
		}
	}()
	lexer.ParseProgram()

	if !lexer.DefinedFunctionsNames.ContainsAll(lexer.CalledFunctionsNames) {
		panic("Undefined function(-s) call(-s) met")
	}

	for functionName, callArgumentsCounters := range lexer.FunctionCallArgumentsCounters {
		for _, callArgumentsCount := range callArgumentsCounters {
			if lexer.FunctionArgumentsNames[functionName].Size() != callArgumentsCount {
				panic("Invalid number of arguments passed to function '" + functionName + "'")
			}
		}
	}

	return true
}

func (lexer *Lexer) ParseProgram() {
	for lexer.Stream.HasNext() {
		lexer.ParseFunction()
		lexer.LineIndex++
	}
}

func (lexer *Lexer) ParseFunction() {
	if lexem := lexer.Stream.Get(); lexem.Tag == VARIABLE {
		functionName := lexem.Image
		if lexer.DefinedFunctionsNames.Contains(functionName) {
			panic("Function with name '" + functionName + "' defined multiple times")
		}

		lexer.CurrentFunctionName = functionName
		lexer.DefinedFunctionsNames.Add(functionName)
		lexer.FunctionArgumentsNames[functionName] = NewSet()
		lexer.Dependencies[functionName] = CalledFunctionsInfo{LineIndex: lexer.LineIndex, FunctionsNames: NewSet()}

		if lexer.Stream.Get().Tag == OPENBRACKET {
			lexer.ParseFormalArgsList()
			if lexer.Stream.Get().Tag == CLOSEBRACKET {
				if lexer.Stream.Get().Tag == COLONEQUAL {
					lexer.ParseExpression()
					if lexer.Stream.Get().Tag == SEMICOLON {
						return
					}
					panic("';' at end of the line expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
				}
				panic("':=' expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
			}
			panic("')' expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
		}
		panic("'(' expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
	}
	panic("Function name declaration expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
}

func (lexer *Lexer) ParseFormalArgsList() {
	if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == VARIABLE {
		lexer.FunctionArgumentsNames[lexer.CurrentFunctionName].Add(lexer.Stream.Get().Image)
		if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == COMMA {
			lexer.Stream.Get()
			if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == VARIABLE {
				lexer.ParseFormalArgsList()
				return
			}
			panic("Variable name after comma expected, but '" + lexer.Stream.Get().Image + "' found")
		}
	}
}

func (lexer *Lexer) ParseExpression() {
	lexer.ParseComparisonExpression()
	if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == QUESTION {
		lexer.Stream.Get()
		lexer.ParseComparisonExpression()
		if lexer.Stream.Get().Tag == COLON {
			lexer.ParseExpression()
			return
		}
		panic("':' expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
	}
}

func (lexer *Lexer) ParseComparisonExpression() {
	lexer.ParseArithmeticalExpression()
	if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag&(EQUAL|NOTEQUAL|LESS|GREATER|LESSEQUAL|GREATEREQUAL) != 0 {
		lexer.Stream.Get()
		lexer.ParseArithmeticalExpression()
	}
}

func (lexer *Lexer) ParseArithmeticalExpression() {
	lexer.ParseTerm()
	if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag&(PLUS|MINUS) != 0 {
		lexer.Stream.Get()
		lexer.ParseArithmeticalExpression()
	}
}

func (lexer *Lexer) ParseTerm() {
	lexer.ParseFactor()
	if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag&(MULT|DIV) != 0 {
		lexer.Stream.Get()
		lexer.ParseTerm()
	}
}

func (lexer *Lexer) ParseFactor() {
	lexem := lexer.Stream.Get()
	switch lexem.Tag {
	case NUMBER:
		return
	case VARIABLE:
		if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == OPENBRACKET {
			lexer.Stream.Get()
			lexer.ProcessFunctionCall(lexem.Image)
		} else if !lexer.FunctionArgumentsNames[lexer.CurrentFunctionName].Contains(lexem.Image) {
			panic("Undefined variable '" + lexem.Image + "'")
		}
	case OPENBRACKET:
		lexer.ParseExpression()
		if lexer.Stream.Get().Tag != CLOSEBRACKET {
			panic("')' expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
		}
	case MINUS:
		lexer.ParseFactor()
	default:
		panic("Unexpected lexem: '" + lexem.Image + "'")
	}
}

func (lexer *Lexer) ProcessFunctionCall(calledFunctionName string) {
	lexer.CalledFunctionsNames.Add(calledFunctionName)
	lexer.Dependencies[lexer.CurrentFunctionName].FunctionsNames.Add(calledFunctionName)
	passedArgumentsCount := lexer.ParseActualArgumentsList()

	if lexer.Stream.Get().Tag != CLOSEBRACKET {
		panic("')' expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
	}

	lexer.FunctionCallArgumentsCounters[calledFunctionName] = append(
		lexer.FunctionCallArgumentsCounters[calledFunctionName],
		passedArgumentsCount)
}

func (lexer *Lexer) ParseActualArgumentsList() int {
	if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == CLOSEBRACKET {
		return 0
	}
	return lexer.ParseExpressionList()
}

func (lexer *Lexer) ParseExpressionList() int {
	var parseExpressionListRecursive func(int) int

	parseExpressionListRecursive = func(counter int) int {
		lexer.ParseExpression()
		if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == COMMA {
			lexer.Stream.Get()
			return parseExpressionListRecursive(counter + 1)
		}
		return counter
	}

	return parseExpressionListRecursive(1)
}

// GRAPH

type Vertex struct {
	FunctionName string
	Edges        []int
	T1           int
	Comp         int
	Low          int
}

func tarjan(graph []Vertex) int {
	var stack []int
	var time, count = 1, 1

	var visitVertex func(int)
	visitVertex = func(index int) {
		graph[index].T1 = time
		graph[index].Low = time
		time++

		stack = append(stack, index)
		for _, uIndex := range graph[index].Edges {
			if graph[uIndex].T1 == 0 {
				visitVertex(uIndex)
			}

			if graph[uIndex].Comp == 0 && graph[index].Low > graph[uIndex].Low {
				graph[index].Low = graph[uIndex].Low
			}
		}
		if graph[index].T1 == graph[index].Low {
			var vIndex int
			for {
				vIndex = stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				graph[vIndex].Comp = count
				if index == vIndex {
					break
				}
			}
			count++
		}
	}
	for i := 0; i < len(graph); i++ {
		if graph[i].T1 == 0 {
			visitVertex(i)
		}
	}

	return count - 1
}

func main() {
	bytesExpression, _ := ioutil.ReadAll(os.Stdin)
	program := string(bytesExpression)
	stream, ok := Tokenize(program)
	if !ok {
		fmt.Println("error")
		os.Exit(1)
	}

	lexer := NewLexer(stream)
	if !lexer.Parse() {
		fmt.Println("error")
		os.Exit(1)
	}

	graph := make([]Vertex, len(lexer.Dependencies))

	for functionName, calledFunctionsInfo := range lexer.Dependencies {
		graph[calledFunctionsInfo.LineIndex] = Vertex{FunctionName: functionName}
	}

	for _, calledFunctionsInfo := range lexer.Dependencies {
		for calledFunctionName := range calledFunctionsInfo.FunctionsNames.Storage {
			index := calledFunctionsInfo.LineIndex
			graph[index].Edges = append(graph[index].Edges, lexer.Dependencies[calledFunctionName].LineIndex)
		}
	}
	fmt.Println(tarjan(graph))
}
