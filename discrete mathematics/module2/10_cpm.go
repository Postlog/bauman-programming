package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Color int

const (
	WHITE Color = iota
	RED
	BLUE
)

type Vertex struct {
	Label       string
	Weight      int
	TotalWeight int

	Edges   []*Vertex
	Parents []*Vertex
	Color   Color

	Visited bool
	ListSum []*Vertex

	Comp int
	T1   int
	Low  int
}

func NewVertex(label string, weight int) *Vertex {
	return &Vertex{Label: label, Weight: weight, Color: WHITE}
}

type VerticesSortedBySum []*Vertex

func (graph VerticesSortedBySum) Len() int {
	return len(graph)
}

func (graph VerticesSortedBySum) Less(i, j int) bool {
	a, b := graph[i].TotalWeight, graph[j].TotalWeight
	return a > b
}

func (graph VerticesSortedBySum) Swap(i, j int) {
	graph[i], graph[j] = graph[j], graph[i]
}

type Queue struct {
	Storage []*Vertex
}

func NewQueue() *Queue {
	return &Queue{[]*Vertex{}}
}

func (queue *Queue) Pop() *Vertex {
	Weight := queue.Storage[0]
	queue.Storage = queue.Storage[1:]
	return Weight
}

func (queue *Queue) Push(Weight *Vertex) {
	queue.Storage = append(queue.Storage, Weight)
}

func (queue Queue) IsEmpty() bool {
	return len(queue.Storage) == 0
}

type Stack struct {
	Storage []*Vertex
}

func NewStack() *Stack {
	return &Stack{[]*Vertex{}}
}

func (stack *Stack) Pop() *Vertex {
	Weight := stack.Storage[len(stack.Storage)-1]
	stack.Storage = stack.Storage[:len(stack.Storage)-1]
	return Weight
}

func (stack *Stack) Push(Weight *Vertex) {
	stack.Storage = append(stack.Storage, Weight)
}

func (stack Stack) IsEmpty() bool {
	return len(stack.Storage) == 0
}

func Tarjan(graph []*Vertex) int {
	stack := NewStack()
	time, count := 1, 1

	var visitVertex func(*Vertex)
	visitVertex = func(vertex *Vertex) {
		vertex.T1 = time
		vertex.Low = time
		time++
		stack.Push(vertex)
		for _, i := range vertex.Edges {
			u := i
			if u.T1 == 0 {
				visitVertex(u)
			}
			if u.Comp == 0 && vertex.Low > u.Low {
				vertex.Low = u.Low
			}
		}
		if vertex.T1 == vertex.Low {
			var u *Vertex
			for {
				u = stack.Pop()
				u.Comp = count
				if vertex == u {
					break
				}
			}
			count++
		}
	}

	for _, vertex := range graph {
		if vertex.T1 == 0 {
			visitVertex(vertex)
		}
	}

	return count
}

func markAsBlueBFS(graph []*Vertex) {
	queue := NewQueue()
	for _, w := range graph {
		if !w.Visited && w.Color == BLUE {
			w.Visited = true
			queue.Push(w)
			for !queue.IsEmpty() {
				v := queue.Pop()
				for _, u := range v.Edges {
					if !u.Visited {
						u.Visited = true
						u.Color = BLUE
						queue.Push(u)
					}
				}
			}
		}
	}
}

func markAsRed(graph []*Vertex) {
	sort.Sort(VerticesSortedBySum(graph))

	maxWeight := graph[0].TotalWeight
	for _, vertex := range graph {
		if vertex.TotalWeight == maxWeight {
			for _, k := range vertex.ListSum {
				k.Color = RED
			}
		} else {
			break
		}
	}
}

func contains(collection []*Vertex, vertex *Vertex) bool {
	for _, u := range collection {
		if u == vertex {
			return true
		}
	}
	return false
}

func displayAsDigraph(graph []*Vertex) {
	fmt.Println("digraph {")
	for _, vertex := range graph {
		fmt.Printf("  %s [label = \"%s(%d)\"", vertex.Label, vertex.Label, vertex.Weight)
		if vertex.Color == RED {
			fmt.Println(", color = red]")
		} else if vertex.Color == BLUE {
			fmt.Println(", color = blue]")
		} else {
			fmt.Println("]")
		}
	}

	for _, vertex := range graph {
		var displayed []*Vertex
		for _, u := range vertex.Edges {
			if contains(displayed, u) {
				continue
			}
			displayed = append(displayed, u)

			fmt.Printf("  %s -> %s", vertex.Label, u.Label)
			if vertex.Color == RED && u.Color == RED && contains(u.Parents, vertex) {
				fmt.Println(" [color = red]")
			} else if vertex.Color == BLUE && u.Color == BLUE {
				fmt.Println(" [color = blue]")
			} else {
				fmt.Println()
			}
		}
	}
	fmt.Println("}")
}

func fillListSumsBFS(graph []*Vertex) {
	queue := NewQueue()
	for _, vertex := range graph {
		if !vertex.Visited && vertex.Color != BLUE {
			vertex.Visited = true
			queue.Push(vertex)
			vertex.ListSum = append(vertex.ListSum, vertex)
			vertex.TotalWeight = vertex.Weight
			for !queue.IsEmpty() {
				v := queue.Pop()
				for _, u := range v.Edges {
					if u.Color == BLUE {
						continue
					}

					if !u.Visited || v.TotalWeight+u.Weight > u.TotalWeight {
						queue.Push(u)
						u.Visited = true
						u.TotalWeight = v.TotalWeight + u.Weight
						u.ListSum = []*Vertex{}
						u.Parents = []*Vertex{}
						u.ListSum = append(u.ListSum, u)
						u.Parents = append(u.Parents, v)

						for _, w := range v.ListSum {
							u.ListSum = append(u.ListSum, w)
						}
					} else if v.TotalWeight+u.Weight == u.TotalWeight {
						queue.Push(u)

						u.Visited = true
						u.Parents = append(u.Parents, v)
						for _, w := range v.ListSum {
							u.ListSum = append(u.ListSum, w)
						}
					}
				}
			}
		}
	}
}

func getComponentsVerticesCount(graph []*Vertex) []int {
	count := Tarjan(graph)
	componentsVerticesCount := make([]int, count)

	for _, vertex := range graph {
		componentsVerticesCount[vertex.Comp-1]++
	}

	return componentsVerticesCount
}

func main() {
	bytesExpression, _ := ioutil.ReadAll(os.Stdin)
	allExpressions := string(bytesExpression)
	allExpressions = strings.ReplaceAll(allExpressions, " ", "")
	allExpressions = strings.ReplaceAll(allExpressions, "\n", "")
	expressionsList := strings.Split(allExpressions, ";")

	labelToVertex := map[string]*Vertex{}
	var graph []*Vertex
	for _, expression := range expressionsList {
		var previousVertex *Vertex = nil
		for _, node := range strings.Split(expression, "<") {
			node = strings.ReplaceAll(node, ")", "")
			tokens := strings.Split(node, "(") // Label1(300) -> [label1 300]; Label1 -> [Label1]
			label := tokens[0]

			if len(tokens) == 1 {
				if previousVertex != nil {
					previousVertex.Edges = append(previousVertex.Edges, labelToVertex[label])
					if previousVertex.Label == label {
						previousVertex.Color = BLUE
					}
				}
				previousVertex = labelToVertex[label]
			} else {
				weight, _ := strconv.Atoi(tokens[1])
				vertex := NewVertex(label, weight)
				if previousVertex != nil {
					previousVertex.Edges = append(previousVertex.Edges, vertex)
				}
				previousVertex = vertex
				labelToVertex[label] = vertex

				graph = append(graph, vertex)
			}
		}
	}

	componentsVerticesCount := getComponentsVerticesCount(graph)
	for _, vertex := range graph {
		if componentsVerticesCount[vertex.Comp-1] > 1 {
			vertex.Color = BLUE
			for _, u := range vertex.Edges {
				u.Color = BLUE
			}
		}
	}

	markAsBlueBFS(graph)

	fillListSumsBFS(graph)
	markAsRed(graph)
	displayAsDigraph(graph)
}
