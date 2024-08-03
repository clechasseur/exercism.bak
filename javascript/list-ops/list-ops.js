export class List {
    constructor(items) {
        this.values = Array.from(items || []);
    }

    length() {
        return this.values.length;
    }

    append(other) {
        const list = new List(this.values);
        for (const item of other.values) {
            list.values.push(item);
        }
        return list;
    }

    concat(other) {
        let list = new List();
        for (const toConcat of [this, other]) {
            for (const item of toConcat.values) {
                if (item instanceof List) {
                    list = list.append(item);
                } else {
                    list.values.push(item);
                }
            }
        }
        return list;
    }

    filter(callback) {
        const list = new List();
        for (let i = 0; i < this.values.length; i++) {
            if (callback(this.values[i], i, this)) {
                list.values.push(this.values[i]);
            }
        }
        return list;
    }

    map(callback) {
        const list = new List();
        for (let i = 0; i < this.values.length; i++) {
            list.values.push(callback(this.values[i], i, this));
        }
        return list;
    }

    foldl(callback, seed) {
        let acc = seed;
        for (let i = 0; i < this.values.length; i++) {
            acc = callback(acc, this.values[i], i, this);
        }
        return acc;
    }

    foldr(callback, seed) {
        let acc = seed;
        for (let i = this.values.length - 1; i >= 0; i--) {
            acc = callback(acc, this.values[i], i, this);
        }
        return acc;
    }

    reverse() {
        const list = new List();
        for (let i = this.values.length - 1; i >= 0; i--) {
            list.values.push(this.values[i]);
        }
        return list;
    }
}
