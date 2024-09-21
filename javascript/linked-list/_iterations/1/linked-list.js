export class LinkedList {
    constructor() {
        this._head = null;
        this._tail = null;
    }

    count() {
        let c = 0;
        let node = this._head;
        while (node !== null) {
            c++;
            node = node.next;
        }
        return c;
    }

    push(element) {
        let node = { value: element, prev: this._tail, next: null };
        if (this._tail !== null) {
            this._tail.next = node;
        }
        this._tail = node;
        if (this._head === null) {
            this._head = this._tail;
        }
    }
    pop() {
        let element = this._tail.value;
        if (this._head === this._tail) {
            this._head = null;
        }
        this._tail = this._tail.prev;
        return element;
    }

    unshift(element) {
        let node = { value: element, prev: null, next: this._head };
        if (this._head !== null) {
            this._head.prev = node;
        }
        this._head = node;
        if (this._tail === null) {
            this._tail = this._head;
        }
    }
    shift() {
        let element = this._head.value;
        if (this._tail === this._head) {
            this._tail = null;
        }
        this._head = this._head.next;
        return element;
    }

    delete(element) {
        let node = this._head;
        while (node !== null) {
            if (node.value === element) {
                if (node.prev !== null) {
                    node.prev.next = node.next;
                }
                if (node.next !== null) {
                    node.next.prev = node.prev;
                }
                if (this._head === node) {
                    this._head = this._head.next;
                }
                if (this._tail === node) {
                    this._tail = this._tail.prev;
                }
                break;
            }
            node = node.next;
        }
    }
}
