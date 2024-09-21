export class Matrix {
    constructor(input) {
        this.rows = input.split('\n').map((line) => line.trim().split(' ').map((i) => Number(i)));
        if (this.rows.length !== 0) {
            this.columns = this.rows[0].map((_, colIdx) => this.rows.map((row) => row[colIdx]));
        } else {
            this.columns = [];
        }

        this.saddlePoints = [];
        for (let rowIdx = 0; rowIdx < this.rows.length; rowIdx++) {
            for (let colIdx = 0; colIdx < this.columns.length; colIdx++) {
                const pointValue = this.rows[rowIdx][colIdx];
                if (this.rows.every((row) => row[colIdx] >= pointValue) && this.columns.every((col) => col[rowIdx] <= pointValue)) {
                    this.saddlePoints.push([rowIdx, colIdx]);
                }
            }
        }
    }
}
