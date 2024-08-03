export class ArgumentError extends Error {
    constructor(...params) {
        super(...params);
        if (Error.captureStackTrace) {
            Error.captureStackTrace(this, ArgumentError);
        }
        this.name = 'ArgumentError';
    }
}

export class WordProblem {
    constructor(question) {
        this.question = question;
    }

    answer() {
        const matches = /^What is (-?\d+)((?: (?:plus|minus|multiplied by|divided by) -?\d+)+)\?$/.exec(this.question);
        if (matches === null) {
            throw new ArgumentError('Wrong question format: ' + this.question);
        }

        let [, seed, ops] = matches;
        const opsRegex = / (plus|minus|multiplied by|divided by) (-?\d+)/y;
        let opMatch;
        let result = Number(seed);
        while ((opMatch = opsRegex.exec(ops)) !== null) {
            const [, op, num] = opMatch;
            if (op === 'plus') {
                result += Number(num);
            } else if (op === 'minus') {
                result -= Number(num);
            } else if (op === 'multiplied by') {
                result *= Number(num);
            } else if (op === 'divided by') {
                result /= Number(num);
            } else {
                throw new ArgumentError('Wrong operator: ' + op);
            }
        }
        return result;
    }
}
