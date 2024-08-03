export class InvalidInputError extends Error {
  constructor(...args) {
    super(...args);
  }
}

export class Robot {
  constructor() {
    this.moves = {
      east: { x: 1, y: 0, left: 'north', right: 'south' },
      west: { x: -1, y: 0, left: 'south', right: 'north' },
      north: { x: 0, y: 1, left: 'west', right: 'east' },
      south: { x: 0, y: -1, left: 'east', right: 'west' },
    };
    this.position = { x: 0, y: 0 };
    this.direction = 'north';
  }

  get bearing() {
    return this.direction;
  }

  get coordinates() {
    return [this.position.x, this.position.y];
  }

  at(x, y) {
    this.place({ x, y, direction: this.direction });
  }

  orient(direction) {
    this.place({ x: this.position.x, y: this.position.y, direction });
  }

  place({ x, y, direction }) {
    if (!(direction in this.moves)) {
      throw new InvalidInputError(`Invalid direction: ${direction}`);
    }
    this.position = { x, y };
    this.direction = direction;
  }

  advance() {
    const move = this.moves[this.direction];
    this.position = { x: this.position.x + move.x, y: this.position.y + move.y };
  }

  turnRight() {
    this.direction = this.moves[this.direction].right;
  }

  turnLeft() {
    this.direction = this.moves[this.direction].left;
  }

  evaluate(input) {
    Robot.instructions(input).forEach((method) => this[method]());
  }

  static instructions(input) {
    return [...input].map((instruction) => {
      if (instruction === 'A') {
        return 'advance';
      } else if (instruction === 'R') {
        return 'turnRight';
      } else if (instruction === 'L') {
        return 'turnLeft';
      } else {
        throw new InvalidInputError(`Invalid instruction: ${instruction}`);
      }
    });
  }
}
