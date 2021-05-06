package main

import (
	"fmt"
	"math"
	"sort"
)

type Edge struct {
	IndexA, IndexB int
	Weight         float64
}

type EdgesAreSorted []Edge

func (edges EdgesAreSorted) Len() int {
	return len(edges)
}

func (edges EdgesAreSorted) Less(i, j int) bool {
	return edges[i].Weight < edges[j].Weight
}

func (edges EdgesAreSorted) Swap(i, j int) {
	edges[i], edges[j] = edges[j], edges[i]
}

type Point2D struct {
	X, Y float64
}

func (point *Point2D) GetDistance(another Point2D) float64 {
	legA, legB := math.Abs(point.X-another.X), math.Abs(point.Y-another.Y)
	return math.Sqrt(math.Pow(legA, 2) + math.Pow(legB, 2))
}

func Kruskal(edges []Edge, ids []int) float64 {
	summaryWeight := 0.0
	counter := 0
	for i := 0; counter < len(ids)-1; i++ {
		indexA, indexB := edges[i].IndexA, edges[i].IndexB
		if ids[indexA] != ids[indexB] {
			counter++
			summaryWeight += edges[i].Weight
			oldId, newId := ids[indexB], ids[indexA]
			for j := 0; j < len(ids); j++ {
				if ids[j] == oldId {
					ids[j] = newId
				}
			}
		}
	}
	return summaryWeight
}

func main() {
	var n int
	fmt.Scan(&n)
	points := make([]Point2D, n)
	for i := range points {
		var x, y float64
		fmt.Scanf("%f %f", &x, &y)

		points[i] = Point2D{x, y}
	}

	edges := make([]Edge, (n-1)*n/2)
	k := 0
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			edges[k] = Edge{i, j, points[i].GetDistance(points[j])}
			k++
		}
	}

	sort.Sort(EdgesAreSorted(edges))

	ids := make([]int, n)
	for i := range ids {
		ids[i] = i
	}
	fmt.Printf("%.2f\n", Kruskal(edges, ids))
}
