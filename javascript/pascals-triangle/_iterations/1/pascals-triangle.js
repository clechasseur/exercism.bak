export class Triangle {
    constructor(numRows) {
        this.rows = [[1]];
        for (let rowIdx = 1; rowIdx < numRows; rowIdx++) {
            const row = [1];
            for (let i = 1; i < rowIdx; i++) {
                row.push(this.rows[rowIdx - 1][i - 1] + this.rows[rowIdx - 1][i]);
            }
            row.push(1);
            this.rows.push(row);
        }
    }
    get lastRow() {
        return this.rows[this.rows.length - 1];
    }
}
