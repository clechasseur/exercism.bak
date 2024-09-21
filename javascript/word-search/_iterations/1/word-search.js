export default class WordSearch {
    constructor(grid) {
        this.grid = grid.map((row) => [...row]);
        this.deltas = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];
    }

    find(words) {
        const result = {};
        for (const word of words) {
            const pos = this.findOne(word);
            if (pos) {
                result[word] = pos;
            }
        }
        return result;
    }

    findOne(word) {
        let pos;
        const splitWord = [...word];
        for (let rowIdx = 0; !pos && rowIdx < this.grid.length; rowIdx++) {
            for (let colIdx = 0; !pos && colIdx < this.grid[rowIdx].length; colIdx++) {
                if (this.grid[rowIdx][colIdx] === splitWord[0]) {
                    for (const delta of this.deltas) {
                        const matchedEnd = this.match(splitWord, [rowIdx, colIdx], delta[0], delta[1]);
                        if (matchedEnd) {
                            pos = {
                                start: [rowIdx + 1, colIdx + 1],
                                end: [matchedEnd[0] + 1, matchedEnd[1] + 1],
                            };
                            break;
                        }
                    }
                }
            }
        }
        return pos;
    }

    match(splitWord, start, rowDelta, colDelta) {
        let pos = start;
        let i = 0;
        while (i < splitWord.length) {
            if (pos[0] < 0 || pos[0] >= this.grid.length || pos[1] < 0 || pos[1] >= this.grid[pos[0]].length) {
                return;
            }
            if (this.grid[pos[0]][pos[1]] !== splitWord[i]) {
                return;
            }
            i++;
            if (i < splitWord.length) {
                pos[0] += rowDelta;
                pos[1] += colDelta;
            }
        }
        return pos;
    }
}
