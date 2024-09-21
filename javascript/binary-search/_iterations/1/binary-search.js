export class BinarySearch {
    constructor(array) {
        if (array.every((e, idx) => idx === 0 || e >= array[idx - 1])) {
            this.array = array;
        }
    }

    indexOf(element, array = this.array, offset = 0) {
        if (array.length === 0) {
            return -1;
        } else {
            const middleIdx = Math.floor(array.length / 2);
            const middleElement = array[middleIdx];
            if (middleElement == element) {
                return middleIdx + offset;
            } else if (element < middleElement) {
                return this.indexOf(element, array.slice(0, middleIdx), offset);
            } else {
                return this.indexOf(element, array.slice(middleIdx + 1), offset + middleIdx + 1);
            }
        }
    }
}
