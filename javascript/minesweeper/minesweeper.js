function* neighbours(row, col, board) {
    const numRows = board.length;
    const numCols = numRows > 0 ? board[0].length : 0;
    for (let r = row - 1; r <= row + 1; r++) {
        for (let c = col - 1; c <= col + 1; c++) {
            if ((r != row || c != col) && r >= 0 && r < numRows && c >= 0 && c < numCols) {
                yield board[r][c];
            }
        }
    }
}

export function annotate(board) {
    return board.map((row, rowIdx) => {
        return Array.from(row).map((c, colIdx) => {
            if (c == ' ') {
                const mineCount = [...neighbours(rowIdx, colIdx, board)].filter((c) => c == '*')
                                                                        .reduce((prev, _) => prev + 1, 0);
                return mineCount > 0 ? mineCount : ' ';
            } else if (c == '*') {
                return '*';
            } else {
                throw new Error('Bad input');
            }
        }).join('');
    });
}
