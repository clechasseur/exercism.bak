// This is a port of my Kotlin solution:
// https://exercism.io/tracks/kotlin/exercises/change/solutions/1ee9ddb205b04ab8a442bb38faa5aff6

export class Change {
    calculate(coins, goal) {
        if (!coins) {
            throw new Error('Must have at least one coin type');
        } else if (!coins.every((c, idx, arr) => idx === 0 || c >= arr[idx - 1])) {
            throw new Error('Coin types must be sorted');
        } else if (coins.some((c) => c <= 0)) {
            throw new Error('Coin types must be greater than 0');
        } else if (!coins.every((c, idx, arr) => idx === 0 || c !== arr[idx - 1])) {
            throw new Error('Coin types must be unique');
        } else if (goal < 0) {
            throw new Error('Negative totals are not allowed.');
        }

        let change = null;
        if (coins[0] === 1) {
            change = this.possiblyCalculate(reverse(coins.slice(1)), goal);
        }
        if (!change) {
            change = this.possiblyCalculate(reverse(coins), goal);
        }
        if (!change) {
            throw new Error(`The total ${goal} cannot be represented in the given currency.`);
        }
        return change.sort((a, b) => a - b);
    }

    possiblyCalculate(coins, goal, steps = Number.MAX_VALUE) {
        if (goal === 0) {
            return [];
        }
        if (steps === 0) {
            return null;
        }

        let change = null;
        coins.forEach((coin) => {
            if (goal >= coin) {
                const subChange = this.possiblyCalculate(
                    dropWhile(coins, (c) => c > coin),
                    goal - coin,
                    Math.min(steps - 1, (change ? change.length : Number.MAX_VALUE) - 1));
                if (subChange && (!change || (subChange.length + 1) < change.length)) {
                    change = [coin, ...subChange];
                }
            }
        });
        return change;
    }
}

function* downwardRange(from, to) {
    for (let i = from; i >= to; i--) {
        yield i;
    }
}

function reverse(arr) {
    return [...downwardRange(arr.length - 1, 0)].map((i) => arr[i]);
}

function dropWhile(arr, pred) {
    const idx = arr.findIndex((e) => !pred(e));
    return idx !== -1 ? arr.slice(idx) : [];
}
