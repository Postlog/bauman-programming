package main

import "fmt"

type Mealy struct {
    StatesCount, AlphabetSize, InitialState int
    TransitionMatrix                        [][]int
    OutputMatrix                            [][]string
}

func NewMealy(StatesCount, AlphabetSize, InitialState int) *Mealy {
    mealy := &Mealy{StatesCount, AlphabetSize, InitialState,
        make([][]int, StatesCount), make([][]string, StatesCount)}

    for i := 0; i < StatesCount; i++ {
        mealy.TransitionMatrix[i] = make([]int, AlphabetSize)
        mealy.OutputMatrix[i] = make([]string, AlphabetSize)
    }

    return mealy
}

func displayMealy(mealy *Mealy) {
    fmt.Println("digraph {")
    fmt.Println("\trankdir = LR")
    fmt.Println("\tdummy [label = \"\", shape = none]")
    for i := 0; i < mealy.StatesCount; i++ {
        fmt.Printf("\t%d [shape = circle]\n", i)
    }
    fmt.Println("\tdummy ->", mealy.InitialState)
    for i := 0; i < mealy.StatesCount; i++ {
        for j := 0; j < mealy.AlphabetSize; j++ {
            fmt.Printf("\t%d -> %d [label = \"%c(%s)\"]\n", i, mealy.TransitionMatrix[i][j], 'a'+j, mealy.OutputMatrix[i][j])
        }
    }
    fmt.Println("}")
}

func main() {
    var statesCount, alphabetSize, initialState int
    fmt.Scanf("%d\n%d\n%d", &statesCount, &alphabetSize, &initialState)
    mealy := NewMealy(statesCount, alphabetSize, initialState)

    for i := 0; i < statesCount; i++ {
        for j := 0; j < alphabetSize; j++ {
            fmt.Scan(&mealy.TransitionMatrix[i][j])
        }
    }

    for i := 0; i < statesCount; i++ {
        for j := 0; j < alphabetSize; j++ {
            fmt.Scan(&mealy.OutputMatrix[i][j])
        }
    }
    displayMealy(mealy)
}