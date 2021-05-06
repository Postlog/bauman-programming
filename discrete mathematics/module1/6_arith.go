package main

import (
	"fmt"
	"github.com/skorobogatov/input"
	"strconv"
)

type Tag int

type Lexem struct {
	Tag
	Image string
}

const (
	ERROR Tag = 1 << iota
	NUMBER
	VAR
	PLUS
	MINUS
	MUL
	DIV
	OBRACKET
	CBRACKET
	UNDEFINED
)

var UNDEFINEDLEXEM = Lexem{UNDEFINED, ""}

type ExpressionChar rune
type ExpressionSlice []ExpressionChar

func (slice ExpressionSlice) iterate(action func(exprSlice ExpressionSlice) int) {
	var offset int
	for i := 0; i < len(slice); {
		if slice[i] == ' ' || slice[i] == '\n' || slice[i] == '\t' {
			i++
			continue
		}

		offset = action(slice[i:])
		if offset == 0 {
			break
		}

		i += offset
	}
}

func (slice ExpressionSlice) iterateEvery(action func(char ExpressionChar) bool) {
	for i := 0; i < len(slice); i++ {
		if slice[i] == ' ' || slice[i] == '\n' || slice[i] == '\t' {
			continue
		}

		if !action(slice[i]) {
			break
		}
	}
}

func (slice ExpressionSlice) asString() string {
	sliceAsString := ""
	for _, char := range slice {
		sliceAsString += char.asString()
	}
	return sliceAsString
}

func (slice ExpressionSlice) tryGetBracket() (Lexem, int) {
	if len(slice) == 0 {
		return UNDEFINEDLEXEM, 0
	}
	char := slice[0]

	if !char.isBracket() {
		return UNDEFINEDLEXEM, 0
	}

	if char == '(' {
		return Lexem{OBRACKET, char.asString()}, 1
	} else {
		return Lexem{CBRACKET, char.asString()}, 1
	}
}

func (slice ExpressionSlice) tryGetVariableName() (Lexem, int) {
	if len(slice) == 0 {
		return UNDEFINEDLEXEM, 0
	}

	if !slice[0].isAlphabetic() {
		return UNDEFINEDLEXEM, 0
	}

	offset := 0

	slice.iterateEvery(func(char ExpressionChar) bool {
		if !char.isAlphabetic() && !char.isNumeric() {
			return false // break from iteration loop
		}

		offset++
		return true
	})

	return Lexem{VAR, slice[:offset].asString()}, offset
}

func (slice ExpressionSlice) tryGetNumericConstant() (Lexem, int) {
	if len(slice) == 0 {
		return UNDEFINEDLEXEM, 0
	}

	if !slice[0].isNumeric() {
		return UNDEFINEDLEXEM, 0
	}

	offset := 0
	wrongLexem := false

	slice.iterateEvery(func(char ExpressionChar) bool {
		if char.isAlphabetic() && !wrongLexem {
			wrongLexem = true
		}

		if !char.isNumeric() && !char.isAlphabetic() {
			return false
		}

		offset++
		return true
	})

	if wrongLexem {
		return Lexem{ERROR, slice[:offset].asString()}, offset
	} else {
		return Lexem{NUMBER, slice[:offset].asString()}, offset
	}
}

func (slice ExpressionSlice) tryGetOperation() (Lexem, int) {
	if len(slice) == 0 {
		return UNDEFINEDLEXEM, 0
	}

	char := slice[0]

	if !char.isOperation() {
		return UNDEFINEDLEXEM, 0
	}
	var operation Tag
	switch char {
	case '+':
		operation = PLUS
	case '-':
		operation = MINUS
	case '*':
		operation = MUL
	case '/':
		operation = DIV
	}

	return Lexem{operation, char.asString()}, 1
}

func (char ExpressionChar) asString() string {
	return string(char)
}

func (char ExpressionChar) isAlphabetic() bool {
	return char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z'
}

func (char ExpressionChar) isNumeric() bool {
	return char >= '0' && char <= '9'
}

func (char ExpressionChar) isSpaceCharacter() bool {
	return char == ' ' || char == '\n' || char == '\t'
}

func (char ExpressionChar) isOperation() bool {
	return char == '+' || char == '-' || char == '/' || char == '*'
}

func (char ExpressionChar) isBracket() bool {
	return char == '(' || char == ')'
}

func lexer(expr string, lexems chan Lexem) {
	ExpressionSlice(expr).iterate(func(exprSlice ExpressionSlice) (indexOffset int) {
		var (
			lexem  Lexem
			offset int
		)

		if lexem, offset = exprSlice.tryGetBracket(); offset > 0 {
			lexems <- lexem
			indexOffset = offset
		} else if lexem, offset = exprSlice.tryGetVariableName(); offset > 0 {
			lexems <- lexem
			indexOffset = offset
		} else if lexem, offset = exprSlice.tryGetNumericConstant(); offset > 0 {
			lexems <- lexem
			indexOffset = offset
		} else if lexem, offset = exprSlice.tryGetOperation(); offset > 0 {
			lexems <- lexem
			indexOffset = offset
		} else {
			panic("unexpected character in expression")
		}

		return
	})

	close(lexems)
}

func analyzer(lexems chan Lexem) (result int, ok bool) {
	defer func() {
		if x := recover(); x != nil {
			result = 0
			ok = false
		}
	}()

	var parseExpression func(bool, int, bool) int
	var parseTerm func(bool, int) int
	var parseFactor func() int

	variables := make(map[string]int)
	watchedLexem := UNDEFINEDLEXEM

	watchLexem := func() Lexem {
		if watchedLexem != UNDEFINEDLEXEM {
			return watchedLexem
		}

		lexem := <-lexems
		watchedLexem = lexem
		return lexem
	}

	getLexem := func() Lexem {
		if watchedLexem == UNDEFINEDLEXEM {
			return <-lexems
		}

		lexem := watchedLexem
		watchedLexem = UNDEFINEDLEXEM
		return lexem
	}

	hasLexems := func() bool {
		if watchedLexem == UNDEFINEDLEXEM {
			lexem, ok := <-lexems
			if !ok {
				return false
			}
			watchedLexem = lexem
			return true
		}

		return true
	}

	parseExpression = func(recursiveCall bool, term int, inBrackets bool) int {
		var T int
		if recursiveCall {
			T = term
		} else {
			T = parseTerm(false, 0)
		}

		if !hasLexems() {
			return T
		}

		if watchLexem().Tag == CBRACKET {
			if inBrackets {
				return T
			} else {
				panic("unexpected close bracket")
			}
		}

		lexem := getLexem()

		if lexem.Tag&(PLUS|MINUS) == 0 {
			panic("lexem not additional operation, lexem=" + lexem.Image)
		}

		T1 := parseTerm(false, 0)

		if lexem.Tag == PLUS {
			return parseExpression(true, T+T1, inBrackets)
		} else {
			return parseExpression(true, T-T1, inBrackets)
		}
	}

	parseTerm = func(recursiveCall bool, factor int) int {
		var F int
		if recursiveCall {
			F = factor
		} else {
			F = parseFactor()
		}

		if !hasLexems() {
			return F
		}

		if watchLexem().Tag&(MUL|DIV) == 0 {
			return F
		}

		lexem := getLexem()

		F1 := parseFactor()

		if lexem.Tag == MUL {
			return parseTerm(true, F*F1)
		} else {
			return parseTerm(true, F/F1)
		}
	}

	parseFactor = func() int {
		lexem := getLexem()

		switch lexem.Tag {
		case ERROR:
			panic("error lexem met")

		case MINUS:
			return -parseFactor()

		case NUMBER:
			value, _ := strconv.Atoi(lexem.Image)
			return value

		case OBRACKET:
			result := parseExpression(false, 0, true)
			if !hasLexems() || getLexem().Tag != CBRACKET {
				panic("no close bracket met")
			}
			return result

		case VAR:
			if value, ok := variables[lexem.Image]; ok {
				return value
			}
			var value int
			input.Scanf("%d", &value)
			variables[lexem.Image] = value
			return value

		default:
			panic("unexpected lexem=" + lexem.Image)
		}
	}
	return parseExpression(false, 0, false), true
}

func main() {
	lexems := make(chan Lexem)
	s := input.Gets()
	go lexer(s, lexems)
	result, ok := analyzer(lexems)
	if !ok {
		fmt.Println("error")
	} else {
		fmt.Println(result)
	}
}
