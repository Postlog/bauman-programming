package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
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

func (set *Set) AsSlice() []string {
	values := make([]string, len(set.Storage))
	i := 0
	for key := range set.Storage {
		values[i] = key
		i++
	}
	return values
}

// -----------------------------------------------

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
	COMMA
	LINEBREAK
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
		case ' ', '\t':
			continue
		case '\n':
			lexem = Lexem{LINEBREAK, ";"}
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

// Dependencies

type RequiredVariablesInfo struct {
	RequiredVariables *Set
	LineIndex         int
}

type DependenciesList struct {
	Storage map[string]RequiredVariablesInfo
}

func NewDependenciesList() *DependenciesList {
	return &DependenciesList{map[string]RequiredVariablesInfo{}}
}

func (list *DependenciesList) FindLineIndex(variableName string) int {
	info, _ := list.Storage[variableName]
	return info.LineIndex
}

func (list *DependenciesList) Add(functionName string, info RequiredVariablesInfo) {
	list.Storage[functionName] = info
}

// LEXER

type Lexer struct {
	DefinedVariableNames, AccessedVariableNames, RightSideVariableNames *Set

	Dependencies *DependenciesList
	Stream       *LexemStream

	CurrentLeftSideVariableName string
	LineIndex                   int
}

func NewLexer(stream *LexemStream) *Lexer {
	return &Lexer{Stream: stream, DefinedVariableNames: NewSet(), AccessedVariableNames: NewSet(), Dependencies: NewDependenciesList()}
}

func (lexer *Lexer) Parse() (result bool) {
	defer func() {
		if x := recover(); x != nil {
			result = false
		}
	}()
	lexer.ParseProgram()

	if !lexer.DefinedVariableNames.ContainsAll(lexer.AccessedVariableNames) {
		panic("Undefined variable(-s) access(-es) met")
	}
	return true
}

func (lexer *Lexer) ParseProgram() {
	for lexer.Stream.HasNext() {
		lexer.ParseLine()
	}
}

func (lexer *Lexer) ParseLine() {
	variableNames := lexer.ParseLeftSideVariables()

	if lexer.Stream.Get().Tag == EQUAL {
		for i, variableName := range variableNames {
			lexer.CurrentLeftSideVariableName = variableName
			lexer.RightSideVariableNames = NewSet()
			lexer.ParseExpression()
			lexer.Dependencies.Add(variableName, RequiredVariablesInfo{
				RequiredVariables: lexer.RightSideVariableNames,
				LineIndex:         lexer.LineIndex,
			})
			if i == len(variableNames)-1 {
				if lexer.Stream.Get().Tag != LINEBREAK {
					panic("New line expected")
				}
				lexer.LineIndex++
				return
			}

			if lexer.Stream.Get().Tag != COMMA {
				panic("',' expected, but '" + lexer.Stream.WatchPrevious().Image + "' found")
			}
		}
	}
	panic("'=' expected, but' " + lexer.Stream.WatchPrevious().Image + "' found")
}

func (lexer *Lexer) ParseLeftSideVariables() []string {
	variableNames := NewSet()
	var parseLeftSideVariablesRecursive func()
	parseLeftSideVariablesRecursive = func() {
		if lexem := lexer.Stream.Get(); lexem.Tag == VARIABLE {
			if lexer.DefinedVariableNames.Contains(lexem.Image) {
				panic("Variable with name '" + lexem.Image + "' defined multiple times")
			}
			lexer.DefinedVariableNames.Add(lexem.Image)

			variableNames.Add(lexem.Image)
			if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == COMMA {
				lexer.Stream.Get()
				if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag == VARIABLE {
					parseLeftSideVariablesRecursive()
					return
				}
				panic("Variable name after comma expected, but '" + lexer.Stream.Get().Image + "' found")
			}
		}
	}
	parseLeftSideVariablesRecursive()
	return variableNames.AsSlice()
}

func (lexer *Lexer) ParseExpression() {
	lexer.ParseTerm()
	if lexer.Stream.HasNext() && lexer.Stream.Watch().Tag&(PLUS|MINUS) != 0 {
		lexer.Stream.Get()
		lexer.ParseExpression()
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
		lexer.RightSideVariableNames.Add(lexem.Image)
		lexer.AccessedVariableNames.Add(lexem.Image)
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

type Color int

const (
	WHITE Color = iota
	GREY
	BLACK
)

type IncidentListNode struct {
	Index int
	Color Color
	edges []IncidentListNode
}

type Vertex struct {
	Color      Color
	Expression string
	Index      int
	Edges      []int
}

func topologicalSortDFS(graph []Vertex) (result []Vertex, ok bool) {
	defer func() {
		if x := recover(); x != nil {
			result = nil
			ok = false
		}
	}()
	var DFSRecursive func(int)

	DFSRecursive = func(index int) {
		graph[index].Color = GREY
		for _, uIndex := range graph[index].Edges {
			if graph[uIndex].Color == WHITE {
				DFSRecursive(uIndex)
			} else if graph[uIndex].Color == GREY {
				panic("Cyclic dependence of variables")
			}
		}

		graph[index].Color = BLACK
		result = append(result, graph[index])
	}

	for i := 0; i < len(graph); i++ {
		if graph[i].Color == WHITE {
			DFSRecursive(i)
		}
	}
	return result, true
}

func main() {
	var (
		allExpressions  string
		expressionsList []string
	)
	bytes, _ := ioutil.ReadAll(os.Stdin)
	allExpressions = string(bytes)
	expressionsList = strings.Split(allExpressions, "\n")

	stream, ok := Tokenize(allExpressions)

	if !ok {
		fmt.Println("syntax error")
		os.Exit(1)
	}

	lexer := NewLexer(stream)
	if !lexer.Parse() {
		fmt.Println("syntax error")
		os.Exit(1)
	}

	graph := make([]Vertex, len(expressionsList))
	for i := 0; i < len(expressionsList); i++ {
		graph[i].Color = WHITE
		graph[i].Expression = expressionsList[i]
		graph[i].Index = i
	}

	for _, requiredVariablesInfo := range lexer.Dependencies.Storage {
		lineIndex := requiredVariablesInfo.LineIndex
		for variableName := range requiredVariablesInfo.RequiredVariables.Storage {
			graph[lineIndex].Edges = append(graph[lineIndex].Edges, lexer.Dependencies.FindLineIndex(variableName))
		}
	}
	verticesQueue, ok := topologicalSortDFS(graph)
	if !ok {
		fmt.Println("cycle")
		os.Exit(1)
	}

	for _, vertex := range verticesQueue {
		fmt.Println(vertex.Expression)
	}
}
