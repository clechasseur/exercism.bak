export class Triangle {
    constructor(side1, side2, side3) {
        this.sides = [side1, side2, side3].sort((a, b) => a - b);
    }

    kind() {
        if (!this.sides.every(n => n > 0) || (this.sides[0] + this.sides[1]) < this.sides[2]) {
            throw new Error('Triangle violates triangle inequality');
        }
        if (this.sides[0] == this.sides[1] && this.sides[1] == this.sides[2]) {
            return 'equilateral';
        } else if (this.sides[0] == this.sides[1] || this.sides[1] == this.sides[2]) {
            return 'isosceles';
        } else {
            return 'scalene';
        }
    }
}
