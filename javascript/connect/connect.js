export class Board {
    constructor(board) {
        this.board = board.map((line) => line.trim().replace(/ /g, ''));

        const Os = this.proliferate(this.topOs(), 'O');
        const Xes = this.proliferate(this.leftXes(), 'X');
        if ([...Os.keys()].some(({ y }) => y === this.board.length - 1)) {
            this._winner = 'O';
        } else if ([...Xes.keys()].some(({ x }) => x === this.board[0].length - 1)) {
            this._winner = 'X';
        } else {
            this._winner = '';
        }
    }

    winner() {
        return this._winner;
    }

    topOs() {
        const Os = new PointSet();
        [...this.board[0]].forEach((type, idx) => {
            if (type === 'O') {
                Os.add({ x: idx, y: 0 });
            }
        })
        return Os;
    }

    leftXes() {
        const Xes = new PointSet();
        this.board.forEach((line, idx) => {
            if (line[0] === 'X') {
                Xes.add({ x: 0, y: idx });
            }
        });
        return Xes;
    }

    proliferate(set, type) {
        let prevSize;
        do {
            prevSize = set.size;
            const newSet = new PointSet(set);
            for (const pt of set) {
                this.sameNeighbours(pt, type).forEach((npt) => {
                    newSet.add(npt);
                });
            }
            set = newSet;
        } while (set.size !== prevSize);
        return set;
    }

    neighbours({ x, y }) {
        return [
            { x, y: y - 1 },
            { x: x + 1, y: y - 1 },
            { x: x - 1, y },
            { x: x + 1, y },
            { x: x - 1, y: y + 1 },
            { x, y: y + 1 },
        ].filter((pt) => this.valid(pt));
    }

    sameNeighbours(pt, type) {
        return this.neighbours(pt).filter(({ x, y }) => this.board[y][x] === type);
    }

    valid({ x, y }) {
        return x >= 0
            && x < this.board[0].length
            && y >= 0
            && y < this.board.length;
    }
}

class PointSet {
    constructor(points) {
        if (points) {
            this.points = [...points];
        } else {
            this.points = [];
        }
    }

    get size() {
        return this.points.length;
    }

    keys() {
        return this.points[Symbol.iterator]();
    }

    [Symbol.iterator]() {
        return this.keys();
    }

    add(pt) {
        if (!this.has(pt)) {
            this.points.push(pt);
        }
    }

    has({ x, y }) {
        return this.points.findIndex(({ x: ex, y: ey }) => ex === x && ey === y) !== -1;
    }
}