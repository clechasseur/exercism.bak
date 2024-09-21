package chessboard

// File stores whether a square is occupied by a piece
type File []bool

// Chessboard is a map of eight Files, accessed with keys from "A" to "H"
type Chessboard map[string]File

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(cb Chessboard, file string) int {
	return countInFile(cb[file])
}

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(cb Chessboard, rank int) (count int) {
	for _, file := range cb {
		if rank >= 1 && rank <= len(file) && file[rank-1] {
			count++
		}
	}
	return
}

// CountAll should count how many squares are present in the chessboard.
func CountAll(cb Chessboard) (count int) {
	for _, file := range cb {
		count += len(file)
	}
	return
}

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(cb Chessboard) (count int) {
	for _, file := range cb {
		count += countInFile(file)
	}
	return
}

func countInFile(file File) (count int) {
	for _, occupied := range file {
		if occupied {
			count++
		}
	}
	return
}
