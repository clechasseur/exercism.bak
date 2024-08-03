export class Element {
    constructor(value) {
        this.value = value;
        this.next = null;
    }
}

export class List {
    constructor(init) {
        this.head = null;
        this.length = 0;
        if (init) {
            for (let value of init) {
                this.add(new Element(value));
            }
        }
    }

    add(element) {
        element.next = this.head;
        this.head = element;
        this.length++;
    }

    toArray() {
        const arr = [];
        let node = this.head;
        while (node !== null) {
            arr.push(node.value);
            node = node.next;
        }
        return arr;
    }

    reverse() {
        return new List(this.toArray());
    }
}
