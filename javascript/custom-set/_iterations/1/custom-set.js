export class CustomSet {
  constructor(elements) {
    this.elements = JSON.parse(JSON.stringify(elements || [])).sort().filter((e, idx, arr) => idx === 0 || arr[idx - 1] != e);
  }

  empty() {
    return this.elements.length === 0;
  }

  contains(element) {
    return this.elements.includes(element);
  }

  add(element) {
    return new CustomSet(this.elements.concat([element]));
  }

  subset(set) {
    return this.elements.every((e) => set.contains(e));
  }

  disjoint(set) {
    return this.elements.every((e) => !set.contains(e));
  }

  eql(set) {
    return this.elements.length === set.elements.length &&
           this.elements.every((e, idx) => e == set.elements[idx]);
  }

  union(set) {
    return new CustomSet(this.elements.concat(set.elements));
  }

  intersection(set) {
    return new CustomSet(this.elements.filter((e) => set.contains(e)));
  }

  difference(set) {
    return new CustomSet(this.elements.filter((e) => !set.contains(e)));
  }
}
