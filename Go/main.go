package main

import (
	"fmt"
	"math"
	"math/rand"
)

func logmod(yinit float64, r float64, k float64, thetasd float64, t int) []float64 {
	ys := make([]float64, t)
	ys[0] = yinit
	for i := 1; i < t; i++ {
		sample := rand.NormFloat64()*thetasd + 0 // mean = 0
		ys[i] = ys[i-1] * (r - r*(ys[i-1]/k)) * math.Exp(sample)
	}
	return ys
}

func main() {
	const YINIT = 1.0   // initial population size
	const R = 1.4       // maximum population growth rate
	const K = 20.0      // carrying capacity
	const THETASD = 0.1 // std dev to add noise

	ys := logmod(YINIT, R, K, THETASD, 5)
	fmt.Println(ys)
}
