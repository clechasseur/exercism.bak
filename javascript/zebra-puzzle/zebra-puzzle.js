import { BEVERAGE, ENGLISHMAN, JAPANESE, NORWEGIAN, PET, SPANIARD, UKRAINIAN, WATER, ZEBRA } from "./domain";
import { Puzzle } from "./puzzle";

// This is a port of my C solution, available at:
// https://exercism.org/tracks/c/exercises/zebra-puzzle/solutions/clechasseur
export class ZebraPuzzle {
  constructor() {
    this.puzzle = new Puzzle();
    this.puzzle.solve();
  }

  waterDrinker() {
    return OWNERS[this.puzzle.ownerMatching(BEVERAGE, WATER)];
  }

  zebraOwner() {
    return OWNERS[this.puzzle.ownerMatching(PET, ZEBRA)];
  }
}

const OWNERS = {
  [ENGLISHMAN]: 'Englishman',
  [SPANIARD]: 'Spaniard',
  [UKRAINIAN]: 'Ukrainian',
  [NORWEGIAN]: 'Norwegian',
  [JAPANESE]: 'Japanese',
};
