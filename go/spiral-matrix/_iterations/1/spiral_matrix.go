package spiralmatrix

func SpiralMatrix(size int) (matrix [][]int) {
	matrix = make([][]int, size)
	for y := range matrix {
		matrix[y] = make([]int, size)
	}

	next := 1
	displacement := pt{1, 0}
	for p := (pt{0, 0}); p.inBounds(size) && matrix[p.y][p.x] == 0; {
		matrix[p.y][p.x] = next
		next += 1

		np := p.advance(displacement)
		if !np.inBounds(size) || matrix[np.y][np.x] != 0 {
			displacement = displacement.turnRight()
			np = p.advance(displacement)
		}

		p = np
	}

	return
}

type pt struct {
	x, y int
}

func (p pt) turnRight() pt {
	return pt{-p.y, p.x}
}

func (p pt) advance(displacement pt) pt {
	return pt{p.x + displacement.x, p.y + displacement.y}
}

func (p pt) inBounds(size int) bool {
	return p.x >= 0 && p.x < size && p.y >= 0 && p.y < size
}
