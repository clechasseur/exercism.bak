const isPalindrome = (num) => [...num.toString()].reverse().join('') === num.toString();

export class Palindromes {
    constructor(smallest, largest) {
        this.smallest = smallest;
        this.largest = largest;
    }

    static generate({ minFactor, maxFactor }) {
        if (minFactor > maxFactor) {
            throw new Error('min must be <= max');
        }

        let smallest = { value: null, factors: [] };
        let largest = { value: null, factors: [] };
        for (let a = minFactor; a <= maxFactor; a++) {
            for (let b = a; b <= maxFactor; b++) {
                const candidate = { value: a * b, factors: [[a, b]] };
                if (isPalindrome(candidate.value)) {
                    if (smallest.value === null || candidate.value < smallest.value) {
                        smallest = candidate;
                    } else if (candidate.value === smallest.value) {
                        smallest.factors.push(candidate.factors[0]);
                    }
                    if (largest.value === null || candidate.value > largest.value) {
                        largest = candidate;
                    } else if (candidate.value === largest.value) {
                        largest.factors.push(candidate.factors[0]);
                    }
                }
            }
        }
        return new Palindromes(smallest, largest);
    }
}
