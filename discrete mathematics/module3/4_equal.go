package main

import (
    "fmt"
)

type DSU struct {
    parent, depth []int
}

func NewDSU(count int) *DSU {
    dsu := &DSU{make([]int, count), make([]int, count)}
    for i := 0; i < count; i++ {
        dsu.parent[i] = i
    }
    return dsu
}

func (dsu *DSU) Find(x int) int {
    if x == dsu.parent[x] {
        return x
    }
    dsu.parent[x] = dsu.Find(dsu.parent[x])
    return dsu.parent[x]
}

func (dsu *DSU) Union(x, y int) {
    x, y = dsu.Find(x), dsu.Find(y)
    if x != y {
        if dsu.depth[x] < dsu.depth[y] {
            x, y = y, x
        }
        dsu.parent[y] = x
        if dsu.depth[x] == dsu.depth[y] {
            dsu.depth[x]++
        }
    }
}

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

func (mealy *Mealy) Split1(m int, roots []int) (int, []int) {
    m = mealy.StatesCount
    dsu := NewDSU(mealy.StatesCount)
    for i := 0; i < mealy.StatesCount; i++ {
        for j := i + 1; j < mealy.StatesCount; j++ {
            if dsu.Find(i) != dsu.Find(j) {
                equal := true
                for k := 0; k < mealy.AlphabetSize; k++ {
                    if mealy.OutputMatrix[i][k] != mealy.OutputMatrix[j][k] {
                        equal = false
                        break
                    }
                }

                if equal {
                    dsu.Union(i, j)
                    m--
                }
            }
        }
    }
    for i := 0; i < mealy.StatesCount; i++ {
        roots[i] = dsu.Find(i)
    }

    return m, roots
}

func (mealy *Mealy) Split(m int, roots []int) (int, []int) {
    m = mealy.StatesCount
    dsu := NewDSU(mealy.StatesCount)
    for i := 0; i < mealy.StatesCount; i++ {
        for j := i + 1; j < mealy.StatesCount; j++ {
            if roots[i] == roots[j] && dsu.Find(i) != dsu.Find(j) {
                equal := true
                for k := 0; k < mealy.AlphabetSize; k++ {
                    if roots[mealy.TransitionMatrix[i][k]] != roots[mealy.TransitionMatrix[j][k]] {
                        equal = false
                        break
                    }
                }
                if equal {
                    dsu.Union(i, j)
                    m--
                }
            }
        }
    }
    for i := 0; i < mealy.StatesCount; i++ {
        roots[i] = dsu.Find(i)
    }

    return m, roots
}

func (mealy *Mealy) MinimizeAufenkampHohn() *Mealy {
    roots := make([]int, mealy.StatesCount)
    var m, m_ int
    m, roots = mealy.Split1(m, roots)
    for {
        m_, roots = mealy.Split(m_, roots)
        if m == m_ {
            break
        }
        m = m_
    }
    a, b := generate(mealy, roots)
    minimized := NewMealy(m, mealy.AlphabetSize, b[roots[mealy.InitialState]])

    for i := 0; i < minimized.StatesCount; i++ {
        for j := 0; j < minimized.AlphabetSize; j++ {
            minimized.TransitionMatrix[i][j] = b[roots[mealy.TransitionMatrix[a[i]][j]]]
            minimized.OutputMatrix[i][j] = mealy.OutputMatrix[a[i]][j]
        }
    }
    return minimized
}

func (mealy *Mealy) Compare(another *Mealy) bool {
    minimizedMealy := mealy.MinimizeAufenkampHohn().GetCanonized()
    minimizedAnother := another.MinimizeAufenkampHohn().GetCanonized()

    if minimizedMealy.StatesCount != minimizedAnother.StatesCount ||
        minimizedMealy.AlphabetSize != minimizedAnother.AlphabetSize ||
        minimizedMealy.InitialState != minimizedAnother.InitialState {
        return false
    }

    for i := 0; i < minimizedMealy.StatesCount; i++ {
        for j := 0; j < minimizedMealy.AlphabetSize; j++ {
            if minimizedMealy.TransitionMatrix[i][j] != minimizedAnother.TransitionMatrix[i][j] ||
                minimizedMealy.OutputMatrix[i][j] != minimizedAnother.OutputMatrix[i][j] {
                return false
            }

        }
    }
    return true
}

func generate(mealy *Mealy, roots []int) (a []int, b []int) {
    a, b = make([]int, mealy.StatesCount), make([]int, mealy.StatesCount)
    counter := 0
    for i := 0; i < mealy.StatesCount; i++ {
        if roots[i] == i {
            a[counter] = i
            b[i] = counter
            counter++
        }
    }
    return
}

func scanMealy(mealy *Mealy) {
    for i := 0; i < mealy.StatesCount; i++ {
        for j := 0; j < mealy.AlphabetSize; j++ {
            fmt.Scan(&mealy.TransitionMatrix[i][j])
        }
    }

    for i := 0; i < mealy.StatesCount; i++ {
        for j := 0; j < mealy.AlphabetSize; j++ {
            fmt.Scan(&mealy.OutputMatrix[i][j])
        }
    }
}

func main() {
    var statesCount, alphabetSize, initialState int
    fmt.Scan(&statesCount, &alphabetSize, &initialState)
    mealyA := NewMealy(statesCount, alphabetSize, initialState)
    scanMealy(mealyA)

    fmt.Scan(&statesCount, &alphabetSize, &initialState)
    mealyB := NewMealy(statesCount, alphabetSize, initialState)
    scanMealy(mealyB)

    if mealyA.Compare(mealyB) {
        fmt.Println("EQUAL")
    } else {
        fmt.Println("NOT EQUAL")
    }
}
