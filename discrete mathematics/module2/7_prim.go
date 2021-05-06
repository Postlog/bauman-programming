package main

import "fmt"

type Vertex struct {
	Edges              []Edge
	PrimKey, PrimIndex int
}

type Edge struct {
	IndexA, IndexB, Length int
}

type PriorityQueue struct {
	Count int
	Heap  []*Vertex
}

func CreatePriorityQueue(size int) PriorityQueue {
	return PriorityQueue{0, make([]*Vertex, size)}
}

func (queue *PriorityQueue) Insert(vertex *Vertex) {
	i := queue.Count
	queue.Heap[queue.Count] = vertex
	queue.Count++
	for ; i > 0 && queue.Heap[(i-1)/2].PrimKey > queue.Heap[i].PrimKey; i = (i - 1) / 2 {
		queue.Heap[(i-1)/2], queue.Heap[i] = queue.Heap[i], queue.Heap[(i-1)/2]
		queue.Heap[i].PrimIndex = i
	}
	queue.Heap[i].PrimIndex = i
}

func (queue *PriorityQueue) ExtractMin() *Vertex {
	vertex := queue.Heap[0]
	queue.Count--
	if queue.Count > 0 {
		queue.Heap[0] = queue.Heap[queue.Count]
		queue.Heap[0].PrimIndex = 0
		queue.SiftDown()
	}
	return vertex
}

func (queue *PriorityQueue) SiftDown() {
	left, right, j, i := 0, 0, 0, 0
	for {
		left = i*2 + 1
		right = left + 1
		j = i
		if left < queue.Count && queue.Heap[i].PrimKey > queue.Heap[left].PrimKey {
			i = left
		}
		if right < queue.Count && queue.Heap[i].PrimKey > queue.Heap[right].PrimKey {
			i = right
		}

		if i == j {
			break
		}

		queue.Heap[i], queue.Heap[j] = queue.Heap[j], queue.Heap[i]
		queue.Heap[i].PrimIndex = i
		queue.Heap[j].PrimIndex = j
	}
}

func (queue *PriorityQueue) DecreaseKey(vertex *Vertex, key int) {
	i := vertex.PrimIndex
	vertex.PrimKey = key
	for ; i > 0 && queue.Heap[(i-1)/2].PrimKey > key; i = (i - 1) / 2 {
		queue.Heap[(i-1)/2], queue.Heap[i] = queue.Heap[i], queue.Heap[(i-1)/2]
		queue.Heap[i].PrimIndex = i
	}
	vertex.PrimIndex = i
}

func MSTPrim(graph []Vertex) int {
	queue := CreatePriorityQueue(len(graph))
	result := 0
	vertex := &graph[0]
	for {
		vertex.PrimIndex = -2
		for i := 0; i < len(vertex.Edges); i++ {
			edge := vertex.Edges[i]
			u := &graph[edge.IndexB]
			if u.PrimIndex == -1 {
				u.PrimKey = edge.Length
				queue.Insert(u)
			} else if u.PrimIndex != -2 && edge.Length < u.PrimKey {
				queue.DecreaseKey(u, edge.Length)
			}
		}
		if queue.Count == 0 {
			break
		}
		vertex = queue.ExtractMin()
		result += vertex.PrimKey
	}
	return result
}

func main() {
	var n, m int
	fmt.Scan(&n)
	fmt.Scan(&m)
	graph := make([]Vertex, n)
	for i := 0; i < n; i++ {
		graph[i].PrimIndex = -1
		graph[i].PrimKey = -1
	}

	for i := 0; i < m; i++ {
		var indexA, indexB, length int
		fmt.Scanf("%d %d %d", &indexA, &indexB, &length)
		graph[indexA].Edges = append(graph[indexA].Edges, Edge{indexA, indexB, length})
		graph[indexB].Edges = append(graph[indexB].Edges, Edge{indexB, indexA, length})
	}

	fmt.Println(MSTPrim(graph))
}
