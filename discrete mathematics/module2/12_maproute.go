package main

import (
	"fmt"
)

const MaxInt = int(^uint(0) >> 1)

type Vertex struct {
	Index, Weight, TotalWeight int
	Edges                      []*Vertex
}

func NewVertex(weight int) *Vertex {
	return &Vertex{Weight: weight, TotalWeight: MaxInt}
}

type PriorityQueue struct {
	Count, Size int
	Heap        []*Vertex
}

func CreatePriorityQueue(size int) PriorityQueue {
	return PriorityQueue{0, size, make([]*Vertex, size)}
}

func (queue *PriorityQueue) IsEmpty() bool {
	return queue.Count == 0
}

func (queue *PriorityQueue) Insert(vertex *Vertex) {
	i := queue.Count
	queue.Count++
	queue.Heap[i] = vertex
	for ; i > 0 && queue.Heap[(i-1)/2].TotalWeight > queue.Heap[i].TotalWeight; i = (i - 1) / 2 {
		queue.Heap[(i-1)/2], queue.Heap[i] = queue.Heap[i], queue.Heap[(i-1)/2]
		queue.Heap[i].Index = i
	}
	queue.Heap[i].Index = i
}

func (queue *PriorityQueue) ExtractMin() *Vertex {
	vertex := queue.Heap[0]
	queue.Count--
	if queue.Count > 0 {
		queue.Heap[0] = queue.Heap[queue.Count]
		queue.Heap[0].Index = 0
		queue.SiftDown()
	}
	return vertex
}

func (queue *PriorityQueue) SiftDown() {
	i := 0
	for {
		left := i*2 + 1
		right := left + 1
		j := i
		if left < queue.Count && queue.Heap[i].TotalWeight > queue.Heap[left].TotalWeight {
			i = left
		}
		if right < queue.Count && queue.Heap[i].TotalWeight > queue.Heap[right].TotalWeight {
			i = right
		}

		if queue.Heap[i].TotalWeight == queue.Heap[j].TotalWeight {
			break
		}
		queue.Heap[i], queue.Heap[j] = queue.Heap[j], queue.Heap[i]
		queue.Heap[i].Index = i
		queue.Heap[j].Index = j
	}
}

func (queue *PriorityQueue) DecreaseKey(vertex *Vertex, key int) {
	i := vertex.Index
	vertex.Weight = key
	for ; i > 0 && queue.Heap[(i-1)/2].TotalWeight > key; i = (i - 1) / 2 {
		queue.Heap[(i-1)/2], queue.Heap[i] = queue.Heap[i], queue.Heap[(i-1)/2]
		queue.Heap[i].Index = i
	}
	vertex.Index = i
}

func relax(v, u *Vertex) bool {
	if v.TotalWeight == MaxInt {
		return false
	}

	if u.TotalWeight > v.TotalWeight+u.Weight {
		u.TotalWeight = v.TotalWeight + u.Weight
		return true
	}
	return false
}

func Dijkstra(queue PriorityQueue) {
	for !queue.IsEmpty() {
		vertex := queue.ExtractMin()
		vertex.Index = -1
		for _, u := range vertex.Edges {
			if u.Index != -1 && relax(vertex, u) {
				queue.DecreaseKey(u, u.TotalWeight)
			}
		}
	}
}

func initEdges(v, u *Vertex) {
	v.Edges = append(v.Edges, u)
	u.Edges = append(u.Edges, v)
}

func main() {
	var n int
	fmt.Scan(&n)
	matrix := make([][]*Vertex, n)
	queue := CreatePriorityQueue(n * n)
	for i := 0; i < n; i++ {
		matrix[i] = make([]*Vertex, n)
		for j := 0; j < n; j++ {
			var weight int
			fmt.Scan(&weight)
			matrix[i][j] = NewVertex(weight)
			if i > 0 {
				initEdges(matrix[i][j], matrix[i-1][j])
			}
			if j > 0 {
				initEdges(matrix[i][j], matrix[i][j-1])
			}
			queue.Insert(matrix[i][j])
		}
	}
	matrix[0][0].TotalWeight = matrix[0][0].Weight
	Dijkstra(queue)
	fmt.Println(matrix[n-1][n-1].TotalWeight)

}
