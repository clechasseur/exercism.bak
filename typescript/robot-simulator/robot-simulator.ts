export class InvalidInputError extends Error {
  constructor(message: string = 'Invalid input') {
    super(message);
  }
}

export type Direction = 'north' | 'east' | 'south' | 'west';
export type Coordinates = [number, number];

const DIRECTIONS: Array<Direction> = ['north', 'east', 'south', 'west'];
const DISPLACEMENTS: Record<Direction, Coordinates> = {
  'north': [0, 1],
  'east': [1, 0],
  'south': [0, -1],
  'west': [-1, 0],
};

function addCoordinates(a: Coordinates, b: Coordinates): Coordinates {
  return [a[0] + b[0], a[1] + b[1]];
}

export class Robot {
  private _bearing: Direction = 'north';
  private _coords: Coordinates = [0, 0];

  get bearing(): Direction {
    return this._bearing;
  }

  get coordinates(): Coordinates {
    return this._coords;
  }

  place({ x, y, direction }: { x: number; y: number; direction: string }) {
    if (DIRECTIONS.indexOf(direction as Direction) === -1) {
      throw new InvalidInputError(`Invalid direction: ${direction}`);
    }

    this._bearing = direction as Direction;
    this._coords = [x, y];
  }

  evaluate(instructions: string) {
    for (const instruction of instructions) {
      switch (instruction) {
        case 'L':
          this.turnRight(3); // UPS-style
          break;
        case 'R':
          this.turnRight(1);
          break;
        case 'A':
          this.advance();
          break;
        default:
          throw new InvalidInputError(`Invalid instruction: ${instruction}`);
      }
    }
  }

  private turnRight(times: number) {
    this._bearing = DIRECTIONS[(DIRECTIONS.indexOf(this._bearing) + times) % DIRECTIONS.length];
  }

  private advance() {
    this._coords = addCoordinates(this._coords, DISPLACEMENTS[this._bearing]);
  }
}
