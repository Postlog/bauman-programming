package main

import (
	"fmt"
	"github.com/skorobogatov/input"
)

type Edge struct {
	IndexA, IndexB int
	IsRed          bool
}

type Vertex struct {
	Index int
	IsRed bool
}

func getMinIndex(component []int) int {
	minIndex := component[0]
	for _, index := range component {
		if index < minIndex {
			minIndex = index
		}
	}
	return minIndex
}

func getComponentEdgesCount(graph [][]int, component []int) int {
	count := 0
	for _, index := range component {
		count += len(graph[index])
	}
	return count / 2
}

func iterateComponentsDFS(graph [][]int, onNewComponent func([]int)) {
	var DFSInner func(int, []int) []int
	visited := make([]bool, len(graph))
	DFSInner = func(index int, component []int) []int {
		visited[index] = true
		component = append(component, index)
		for _, uIndex := range graph[index] {
			if visited[uIndex] == false {
				component = DFSInner(uIndex, component)
			}
		}
		return component
	}
	for i := range graph {
		if visited[i] == false {
			component := DFSInner(i, []int{})
			onNewComponent(component)
		}
	}
}

func getMaxComponent(graph [][]int) []int {
	var maxComponent []int
	iterateComponentsDFS(graph, func(component []int) {
		if len(component) > len(maxComponent) {
			maxComponent = component
			return
		}
		componentEdgesCount := getComponentEdgesCount(graph, component)
		maxComponentEdgesCount := getComponentEdgesCount(graph, maxComponent)
		if len(component) == len(maxComponent) && componentEdgesCount > maxComponentEdgesCount {
			maxComponent = component
			return
		}

		if len(component) == len(maxComponent) &&
			componentEdgesCount == maxComponentEdgesCount &&
			getMinIndex(component) < getMinIndex(maxComponent) {
			maxComponent = component
			return
		}
	})
	return maxComponent
}

func displayAsDot(vertices []Vertex, edges []Edge) {
	fmt.Println("graph {")
	for _, vertex := range vertices {
		fmt.Print("\t")
		if vertex.IsRed == true {
			fmt.Println(vertex.Index, "[color = red]")
		} else {
			fmt.Println(vertex.Index)
		}
	}

	for _, edge := range edges {
		fmt.Print("\t")
		if edge.IsRed == true {
			fmt.Println(edge.IndexA, "--", edge.IndexB, "[color = red]")
		} else {
			fmt.Println(edge.IndexA, "--", edge.IndexB)
		}
	}

	fmt.Println("}")
}

func main() {
	var verticesCount, edgesCount int
	input.Scanf("%d \n %d", &verticesCount, &edgesCount)

	graph := make([][]int, verticesCount)

	vertices := make([]Vertex, verticesCount)
	edges := make([]Edge, edgesCount)

	for i := 0; i < verticesCount; i++ {
		graph[i] = []int{}
		vertices[i] = Vertex{i, false}
	}

	for i := 0; i < edgesCount; i++ {
		var indexA, indexB int
		input.Scanf("%d %d", &indexA, &indexB)
		edges[i] = Edge{indexA, indexB, false}
		graph[indexA] = append(graph[indexA], indexB)
		graph[indexB] = append(graph[indexB], indexA)
	}

	maxComponent := getMaxComponent(graph)

	for _, index := range maxComponent {
		vertices[index].IsRed = true
	}

	for i := 0; i < edgesCount; i++ {
		indexA := edges[i].IndexA
		if vertices[indexA].IsRed == true {
			edges[i].IsRed = true
		}
	}

	displayAsDot(vertices, edges)
}
