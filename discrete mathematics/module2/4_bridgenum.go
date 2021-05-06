package main

import "fmt"

type Vertex struct {
	Edges []int
}

type VertexBypassInfo struct {
	Visited bool
	TimeIn  int
	Time    int
}

func min(a, b int) int {
	if a > b {
		return b
	}
	return a
}

func countBridgesDFS(graph []Vertex) int {
	bridgesCounter := 0
	time := 0
	info := make([]VertexBypassInfo, len(graph))

	var DFSInner func(int, int)
	DFSInner = func(index int, previousIndex int) {
		info[index].Visited = true
		info[index].TimeIn = time
		info[index].Time = time
		time++

		for _, uIndex := range graph[index].Edges {
			if uIndex == previousIndex {
				continue
			}
			if info[uIndex].Visited == true {
				info[index].Time = min(info[index].Time, info[uIndex].TimeIn)
			} else {
				DFSInner(uIndex, index)
				info[index].Time = min(info[index].Time, info[uIndex].Time)
				if info[uIndex].Time > info[index].TimeIn {
					bridgesCounter++
				}
			}
		}
	}
	for i := range graph {
		if info[i].Visited == false {
			DFSInner(i, -1)
		}
	}
	return bridgesCounter
}

func main() {
	var n, m int
	fmt.Scanf("%d\n%d", &n, &m)
	graph := make([]Vertex, n)
	for i := 0; i < m; i++ {
		var indexA, indexB int
		fmt.Scanf("%d %d", &indexA, &indexB)
		graph[indexA].Edges = append(graph[indexA].Edges, indexB)
		graph[indexB].Edges = append(graph[indexB].Edges, indexA)
	}
	fmt.Println(countBridgesDFS(graph))
}
