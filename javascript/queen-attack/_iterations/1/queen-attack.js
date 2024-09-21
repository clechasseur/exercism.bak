function samePositions(a, b) {
    return a[0] == b[0] && a[1] == b[1];
}

export class QueenAttack {
    constructor(positions = { white: [0, 3], black: [7, 3] }) {
        if (samePositions(positions.white, positions.black)) {
            throw 'Queens cannot share the same space';
        }
        this.white = positions.white;
        this.black = positions.black;
    }

    toString() {
        let board = '';
        for (let rowIdx = 0; rowIdx < 8; rowIdx++) {
            let row = '';
            for (let colIdx = 0; colIdx < 8; colIdx++) {
                if (row.length != 0) {
                    row += ' ';
                }
                if (samePositions(this.white, [rowIdx, colIdx])) {
                    row += 'W';
                } else if (samePositions(this.black, [rowIdx, colIdx])) {
                    row += 'B';
                } else {
                    row += '_';
                }
            }
            board += row + '\n';
        }
        return board;
    }

    canAttack() {
        return this.white[0] == this.black[0] ||
               this.white[1] == this.black[1] ||
               Math.abs(this.white[0] - this.black[0]) == Math.abs(this.white[1] - this.black[1]);
    }
}
