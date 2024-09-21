import { isNull, isUndefined } from "util";

export class Flattener {
  flatten(element) {
    return this.flattenTo(element, []);
  }

  flattenTo(element, destination) {
    if (!Array.isArray(element)) {
      if (!isNull(element) && !isUndefined(element)) {
        destination.push(element);
      }
    } else {
      for (const subElement of element) {
        this.flattenTo(subElement, destination);
      }
    }
    return destination;
  }
}
