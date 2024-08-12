function* coords(startX, maxX, startY, maxY) {
    for (let x = startX; x < maxX; x++) {
        for (let y = startY; y < maxY; y++) {
            yield [x, y];
        }
    }
}

function horizLine(matrix, x, startY, endY) {
    return matrix[x][startY] === '+' &&
           matrix[x][endY] === '+' &&
           matrix[x].slice(startY + 1, endY).every((c) => c === '-' || c === '+');
}
function vertLine(matrix, y, startX, endX) {
    return matrix[startX][y] === '+' &&
           matrix[endX][y] === '+' &&
           matrix.slice(startX + 1, endX).every((row) => row[y] === '|' || row[y] ==='+');
}
function rectLines(matrix, startX, startY, endX, endY) {
    return horizLine(matrix, startX, startY, endY) &&
           horizLine(matrix, endX, startY, endY) &&
           vertLine(matrix, startY, startX, endX) &&
           vertLine(matrix, endY, startX, endX);
}

export const Rectangles = {
    count: (rows) => {
        const matrix = rows.map((row) => [...row]);
        const height = matrix.length;
        const width = height ? matrix[0].length : 0;
        let count = 0;
        for (const [x, y] of coords(0, height, 0, width)) {
            if (matrix[x][y] === '+') {
                for (const [candX, candY] of coords(x + 1, height, y + 1, width)) {
                    if (matrix[candX][candY] === '+' && rectLines(matrix, x, y, candX, candY)) {
                        count++;
                    }
                }
            }
        }
        return count;
    }
};
