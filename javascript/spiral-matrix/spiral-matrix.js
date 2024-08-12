function* zeroes(n) {
    for (let i = 0; i < n; i++) {
        yield 0;
    }
}
function* zeroedArrays(n) {
    for (let i = 0; i < n; i++) {
        yield [...zeroes(n)];
    }
}

const DIRECTION = {
    RIGHT: 0, DOWN: 1, LEFT: 2, UP: 3,
};
class Mouse {
    constructor(matrix, height, width) {
        this.matrix = matrix;
        this.height = height;
        this.width = width;
        this.x = 0;
        this.y = 0;
        this.dir = DIRECTION.RIGHT;
    }
    turn() {
        if (this.dir == DIRECTION.RIGHT) {
            this.dir = DIRECTION.DOWN;
        } else if (this.dir == DIRECTION.DOWN) {
            this.dir = DIRECTION.LEFT;
        } else if (this.dir == DIRECTION.LEFT) {
            this.dir = DIRECTION.UP;
        } else {
            this.dir = DIRECTION.RIGHT;
        }
    }
    tryAdvance() {
        return {
            x: this.dir == DIRECTION.LEFT ? this.x - 1 : (this.dir == DIRECTION.RIGHT ? this.x + 1 : this.x),
            y: this.dir == DIRECTION.UP ? this.y - 1 : (this.dir == DIRECTION.DOWN ? this.y + 1 : this. y),
        };
    }
    move() {
        const { x: newX, y: newY } = this.tryAdvance();
        if (newX < 0 || newX >= this.width || newY < 0 || newY >= this.height || this.matrix[newY][newX] != 0) {
            this.turn();
            ({ x: this.x, y: this.y } = this.tryAdvance());
        } else {
            this.x = newX;
            this.y = newY;
        }
    }
}

export const SpiralMatrix = {
    ofSize: (size) => {
        if (size === 0) {
            return [];
        }
        const matrix = [...zeroedArrays(size)];
        const height = matrix.length;
        const width = height ? matrix[0].length : 0;
        const mouse = new Mouse(matrix, height, width);
        let n = 1;
        while (mouse.x >= 0 && mouse.x < width && mouse.y >= 0 && mouse.y < height && matrix[mouse.y][mouse.x] === 0) {
            matrix[mouse.y][mouse.x] = n++;
            mouse.move();
        }
        return matrix;
    }
};
