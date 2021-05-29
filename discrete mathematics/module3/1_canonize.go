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

func (mealy *Mealy) GetCanonicalNumbering() ([]int, int, []bool) {
    visited := make([]bool, mealy.StatesCount)
    numbering := make([]int, mealy.StatesCount)
    for i := range numbering {
        numbering[i] = -1
    }

    count := 0

    var DFSInner func(int)
    DFSInner = func(index int) {
        numbering[index] = count
        count++
        visited[index] = true
        for i := 0; i < mealy.AlphabetSize; i++ {
            if !visited[mealy.TransitionMatrix[index][i]] {
                DFSInner(mealy.TransitionMatrix[index][i])
            }
        }
    }
    DFSInner(mealy.InitialState)
    return numbering, count, visited
}

func (mealy *Mealy) GetCanonized() *Mealy {
    canonized := NewMealy(mealy.StatesCount, mealy.AlphabetSize, 0)
    numbering, lastIndex, visited := mealy.GetCanonicalNumbering()
    canonized.StatesCount = lastIndex
    for i := 0; i < mealy.StatesCount; i++ {
        if visited[i] && numbering[i] != -1 {
            canonized.OutputMatrix[numbering[i]] = mealy.OutputMatrix[i]
            for j := 0; j < mealy.AlphabetSize; j++ {
                canonized.TransitionMatrix[numbering[i]][j] = numbering[mealy.TransitionMatrix[i][j]]
            }
        }
    }
    return canonized
}

func displayMealy(mealy *Mealy) {
    fmt.Printf("%d\n%d\n%d\n", mealy.StatesCount, mealy.AlphabetSize, mealy.InitialState)
    for i := 0; i < mealy.StatesCount; i++ {
        for j := 0; j < mealy.AlphabetSize; j++ {
            fmt.Print(mealy.TransitionMatrix[i][j], " ")
        }
        fmt.Println("")
    }

    for i := 0; i < mealy.StatesCount; i++ {
        for j := 0; j < mealy.AlphabetSize; j++ {
            fmt.Print(mealy.OutputMatrix[i][j], " ")
        }
        fmt.Println("")
    }
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

    displayMealy(mealy.GetCanonized())
}
