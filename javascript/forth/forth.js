export class Forth {
    constructor() {
        this.stack = []
        this.ops = {
            '+': () => {
                const [first, second] = this.pop(2);
                this.push(first + second);
            },
            '-': () => {
                const [first, second] = this.pop(2);
                this.push(first - second);
            },
            '*': () => {
                const [first, second] = this.pop(2);
                this.push(first * second);
            },
            '/': () => {
                const [first, second] = this.pop(2);
                if (second === 0) {
                    throw new Error('Division by zero');
                }
                this.push(Math.floor(first / second));
            },
            'dup': () => {
                const [value] = this.pop(1);
                this.push(value, value);
            },
            'drop': () => {
                this.pop(1);
            },
            'swap': () => {
                const [first, second] = this.pop(2);
                this.push(second, first);
            },
            'over': () => {
                const [first, second] = this.pop(2);
                this.push(first, second, first);
            },
        };
    }

    evaluate(input) {
        if (input) {
            let [next, rest] = this.splitInput(input);
            if (next.toLowerCase() in this.ops) {
                this.ops[next.toLowerCase()]();
            } else if (!isNaN(Number(next))) {
                this.stack.push(Number(next));
            } else if (next === ':') {
                let alias, nextop;
                let ops = "";
                [alias, rest] = this.splitInput(rest);
                if (!isNaN(Number(alias))) {
                    throw new Error('Invalid definition');
                }
                [nextop, rest] = this.splitInput(rest);
                while (nextop !== ';') {
                    ops = `${ops} ${nextop.toLowerCase()}`;
                    [nextop, rest] = this.splitInput(rest);
                }
                ops = ops.trim();
                this.ops[alias.toLowerCase()] = () => {
                    this.evaluate(ops);
                }
            } else {
                throw new Error('Unknown command');
            }

            this.evaluate(rest);
        }
    }

    push(...values) {
        this.stack.push(...values);
    }

    pop(num) {
        if (this.stack.length < num) {
            this.stack = [];
            throw new Error('Stack empty');
        }
        return this.stack.splice(this.stack.length - num, num);
    }

    splitInput(input) {
        return input.match(/^([^ ]+)(?: +(.*))?$/).slice(1, 3);
    }
}
