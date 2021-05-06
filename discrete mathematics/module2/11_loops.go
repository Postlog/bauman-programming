package main

import (
	"fmt"
	"sort"
)

type Vertex struct {
	IngoingEdges, Edges, Bucket                       []*Vertex
	Ancestor, Label, SemiDominator, Dominator, Parent *Vertex
	Visited, HasOperand                               bool
	Depth, OperandValue                               int
}

func NewVertex() *Vertex {
	vertex := &Vertex{}
	vertex.SemiDominator = vertex
	vertex.Label = vertex
	return vertex
}

type VerticesAreSorted []*Vertex

func (graph VerticesAreSorted) Len() int {
	return len(graph)
}

func (graph VerticesAreSorted) Less(i, j int) bool {
	a, b := graph[i].Depth, graph[j].Depth
	return a < b
}

func (graph VerticesAreSorted) Swap(i, j int) {
	graph[i], graph[j] = graph[j], graph[i]
}

func visitVerticesDFS(start *Vertex) {
	depth := 0

	var DFSInner func(*Vertex)
	DFSInner = func(vertex *Vertex) {
		vertex.Depth = depth
		vertex.Visited = true
		depth++

		for _, u := range vertex.Edges {
			if u.Visited == false {
				u.Parent = vertex
				DFSInner(u)
			}
		}
	}
	DFSInner(start)
}

func filterVertices(graph *[]*Vertex) {
	visitVerticesDFS((*graph)[0])
	var filteredGraph []*Vertex
	for _, vertex := range *graph {
		if vertex.Visited {
			filteredGraph = append(filteredGraph, vertex)
		}
	}
	*graph = filteredGraph

	for _, vertex := range *graph {
		var filteredIngoingEdges []*Vertex
		for _, u := range vertex.IngoingEdges {
			if u.Visited {
				filteredIngoingEdges = append(filteredIngoingEdges, u)
			}
		}
		vertex.IngoingEdges = filteredIngoingEdges
	}
}

func dominators(graph []*Vertex) {
	sort.Sort(VerticesAreSorted(graph))
	for i := len(graph) - 1; i > 0; i-- {
		vertex := graph[i]
		for _, u := range vertex.IngoingEdges {
			minVertex := findMin(u)
			if minVertex.SemiDominator.Depth < vertex.SemiDominator.Depth {
				vertex.SemiDominator = minVertex.SemiDominator
			}
		}
		vertex.Ancestor = vertex.Parent
		vertex.SemiDominator.Bucket = append(vertex.SemiDominator.Bucket, vertex)
		for _, u := range vertex.Parent.Bucket {
			minVertex := findMin(u)
			if minVertex.SemiDominator == u.SemiDominator {
				u.Dominator = vertex.Parent
			} else {
				u.Dominator = minVertex
			}
		}
		vertex.Parent.Bucket = nil
	}

	for i := 1; i < len(graph); i++ {
		if graph[i].Dominator != graph[i].SemiDominator {
			graph[i].Dominator = graph[i].Dominator.Dominator
		}
	}

	graph[0].Dominator = nil
}

func findMin(vertex *Vertex) *Vertex {
	searchAndCut(vertex)
	return vertex.Label
}

func searchAndCut(vertex *Vertex) *Vertex {
	if vertex.Ancestor == nil {
		return vertex
	}
	root := searchAndCut(vertex.Ancestor)
	if vertex.Ancestor.Label.SemiDominator.Depth < vertex.Label.SemiDominator.Depth {
		vertex.Label = vertex.Ancestor.Label
	}
	vertex.Ancestor = root
	return root
}

func getLoopsCount(graph []*Vertex) int {
	loopsCount := 0
	for _, vertex := range graph {
		for _, u := range vertex.IngoingEdges {
			for u != vertex && u != nil {
				u = u.Dominator
			}
			if u == vertex {
				loopsCount++
				break
			}
		}
	}
	return loopsCount
}

func linkVertex(graph []*Vertex, index int) {
	if index < len(graph)-1 {
		graph[index].Edges = append(graph[index].Edges, graph[index+1])
		graph[index+1].IngoingEdges = append(graph[index+1].IngoingEdges, graph[index])
	}
}

func linkOperands(graph []*Vertex, indexToVertex map[int]*Vertex) {
	for _, vertex := range graph {
		if !vertex.HasOperand {
			continue
		}
		vertex.Edges = append(vertex.Edges, indexToVertex[vertex.OperandValue])
		indexToVertex[vertex.OperandValue].IngoingEdges = append(indexToVertex[vertex.OperandValue].IngoingEdges, vertex)
	}
}

func main() {
	var n int
	fmt.Scan(&n)
	graph := make([]*Vertex, n)
	for i := 0; i < n; i++ {
		graph[i] = NewVertex()
	}

	indexToVertex := map[int]*Vertex{}
	for i := 0; i < n; i++ {
		var index int
		var command string
		fmt.Scan(&index, &command)
		indexToVertex[index] = graph[i]
		if command == "ACTION" {
			linkVertex(graph, i)
		} else {
			var operand int
			fmt.Scan(&operand)
			graph[i].HasOperand = true
			graph[i].OperandValue = operand
			if command == "BRANCH" {
				linkVertex(graph, i)
			}
		}
	}

	linkOperands(graph, indexToVertex)
	filterVertices(&graph)
	dominators(graph)
	fmt.Println(getLoopsCount(graph))
}
