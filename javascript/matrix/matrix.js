export class Matrix {
    constructor(input) {
        this.rows = input.split('\n').map((row) => {
            return row.split(' ').map((num) => Number(num));
        });
        this.columns = [...Array(this.rows[0].length).keys()].map((colIdx) => {
            return this.rows.map((row) => row[colIdx]);
        });
    }
}
