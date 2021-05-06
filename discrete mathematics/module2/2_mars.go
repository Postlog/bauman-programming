package main

import (
	"fmt"
	"math"
	"sort"
)

type Vertex struct {
	Edges        []*Vertex
	Group, Index int
	Visited      bool
}

type VerticesAreSorted []*Vertex

func (vertices VerticesAreSorted) Len() int {
	return len(vertices)
}

func (vertices VerticesAreSorted) Less(i, j int) bool {
	a, b := vertices[i].Index, vertices[j].Index
	return a < b
}

func (vertices VerticesAreSorted) Swap(i, j int) {
	vertices[i], vertices[j] = vertices[j], vertices[i]
}

type Division struct {
	Balance           int
	LeftSideVertices  *Set
	RightSideVertices *Set
}

type Set struct {
	Storage map[*Vertex]bool
}

func NewSet() *Set {
	return &Set{map[*Vertex]bool{}}
}

func (set *Set) Add(value *Vertex) {
	set.Storage[value] = true
}

func (set *Set) Size() int {
	return len(set.Storage)
}

func (set *Set) AsSlice() []*Vertex {
	values := make([]*Vertex, len(set.Storage))
	i := 0
	for key := range set.Storage {
		values[i] = key
		i++
	}
	return values
}

func (set *Set) Compare(another *Set) bool {
	if set.Size() != another.Size() {
		return set.Size() < another.Size()
	}
	sliceA, sliceB := set.AsSlice(), another.AsSlice()
	sort.Sort(VerticesAreSorted(sliceA))
	sort.Sort(VerticesAreSorted(sliceB))
	for i := 0; i < len(sliceA); i++ {
		if sliceA[i].Index < sliceB[i].Index {
			return true
		}
		if sliceA[i].Index > sliceB[i].Index {
			return false
		}
	}
	return false
}

func DFS(start *Vertex, component *Division) (result bool) {
	defer func() {
		if x := recover(); x != nil {
			result = false
		}
	}()
	var DFSInner func(*Vertex)
	DFSInner = func(vertex *Vertex) {
		vertex.Visited = true
		if vertex.Group == -1 {
			component.LeftSideVertices.Add(vertex)
		} else {
			component.RightSideVertices.Add(vertex)
		}
		component.Balance += vertex.Group
		for _, u := range vertex.Edges {
			if vertex.Group*u.Group == 1 {
				panic("no solution")
			}
			if !u.Visited {
				u.Group = -vertex.Group
				DFSInner(u)
			}
		}
	}
	DFSInner(start)
	return true
}

func generateMasks(n int) [][]bool {
	var masks [][]bool
	var generateMasksInner func([]bool, int)
	generateMasksInner = func(mask []bool, i int) {
		if i == n {
			masks = append(masks, mask)
			return
		}
		maskWithTrueEnding := make([]bool, len(mask)+1)
		maskWithFalseEnding := make([]bool, len(mask)+1)
		for j, value := range mask {
			maskWithTrueEnding[j] = value
			maskWithFalseEnding[j] = value
		}
		maskWithTrueEnding[len(mask)] = true
		maskWithFalseEnding[len(mask)] = false
		generateMasksInner(maskWithTrueEnding, i+1)
		generateMasksInner(maskWithFalseEnding, i+1)
	}

	generateMasksInner([]bool{}, 0)
	return masks
}

func main() {
	var n int
	fmt.Scan(&n)

	graph := make([]*Vertex, n)
	for i := 0; i < n; i++ {
		graph[i] = &Vertex{Index: i}
	}

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			var connection string
			fmt.Scan(&connection)
			if connection[0] == '+' {
				graph[i].Edges = append(graph[i].Edges, graph[j])
			}
		}
	}

	var divisions []*Division
	for _, vertex := range graph {
		if !vertex.Visited {
			vertex.Group = 1
			component := &Division{LeftSideVertices: NewSet(), RightSideVertices: NewSet()}
			divisions = append(divisions, component)
			if !DFS(vertex, component) {
				fmt.Println("No solution")
				return
			}
		}
	}

	masks := generateMasks(len(divisions))
	sumsOfMasks := make([]int, len(masks))
	minimalSum := n
	for i := 0; i < len(masks); i++ {
		sum := 0
		for j := 0; j < len(divisions); j++ {
			if !masks[i][j] {
				sum -= divisions[j].Balance
			} else {
				sum += divisions[j].Balance
			}
		}
		sum = int(math.Abs(float64(sum)))

		if sum < minimalSum {
			minimalSum = sum
		}
		sumsOfMasks[i] = sum
	}
	minimals := NewSet()
	for i := 0; i < len(masks); i++ {
		if sumsOfMasks[i] == minimalSum {
			mergedSet := NewSet()
			for j := 0; j < len(divisions); j++ {
				subset := divisions[j].RightSideVertices
				if masks[i][j] {
					subset = divisions[j].LeftSideVertices
				}
				for key := range subset.Storage {
					mergedSet.Add(key)
				}
			}
			if minimals.Size() == 0 || mergedSet.Compare(minimals) {
				minimals = mergedSet
			}
		}
	}

	minimalsSlice := minimals.AsSlice()
	sort.Sort(VerticesAreSorted(minimalsSlice))
	for _, vertex := range minimalsSlice {
		fmt.Print(vertex.Index+1, " ")
	}
	fmt.Println()
}
