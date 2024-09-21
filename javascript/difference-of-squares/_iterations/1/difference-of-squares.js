export class Squares {
    constructor(n) {
        this.squareOfSum = [...Array(n).keys()].map(i => i + 1).reduce((acc, i) => acc + i) ** 2;
        this.sumOfSquares = [...Array(n).keys()].map(i => (i + 1) ** 2).reduce((acc, i) => acc + i);
        this.difference = this.squareOfSum - this.sumOfSquares;
    }
}
