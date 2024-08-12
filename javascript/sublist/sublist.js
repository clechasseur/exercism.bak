export class List {
  constructor(elements = []) {
    this.elements = elements;
  }

  compare(other) {
    const superlist = this.isSuperlistOf(other);
    const sublist = other.isSuperlistOf(this);
    if (superlist && sublist) {
      return 'EQUAL';
    } else if (superlist) {
      return 'SUPERLIST';
    } else if (sublist) {
      return 'SUBLIST';
    } else {
      return 'UNEQUAL';
    }
  }

  isSuperlistOf(other) {
    return this.elements.length >= other.elements.length &&
           (other.elements.length === 0 || [...this.elements.keys()].slice(0, this.elements.length - other.elements.length + 1)
                                                                    .some((i) => this.elements.slice(i, i + other.elements.length)
                                                                                              .every((e, idx) => e == other.elements[idx])));
  }
}
