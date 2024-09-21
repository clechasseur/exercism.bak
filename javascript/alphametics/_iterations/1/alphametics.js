function validPermutation(perm, edgeLetters) {
    let valid = true;
    for (const c in perm) {
        for (const oc in perm) {
            if (c !== oc && perm[c] === perm[oc]) {
                valid = false;
                break;
            }
        }
        if (!valid) {
            break;
        }
    }
    return valid && edgeLetters.every((c) => perm[c] !== 0);
}

function* permutations(letters, edgeLetters) {
    let perm = {};
    letters.forEach((c) => perm[c] = 0);
    for (;;) {
        if (validPermutation(perm, edgeLetters)) {
            yield perm;
        }
        
        let nineCount = 0;
        for (const c in perm) {
            if (perm[c] === 9) {
                nineCount++;
            }
        }
        if (nineCount === letters.length) {
            break;
        }

        let i = 0;
        while (perm[letters[i]] === 9) {
            perm[letters[i]] = 0;
            i++;
        }
        perm[letters[i]]++;
    }
}

export function solve(puzzle) {
    const letters = [];
    const edgeLetters = [];
    let edge = true;
    [...puzzle].forEach((c) => {
        if (/^[A-Z]$/.test(c)) {
            if (!letters.includes(c)) {
                letters.push(c);
            }
            if (edge && !edgeLetters.includes(c)) {
                edgeLetters.push(c);
            }
            edge = false;
        } else {
            edge = true;
        }
    });
    letters.sort();
    edgeLetters.sort();

    for (const perm of permutations(letters, edgeLetters)) {
        const variant = puzzle.replace(/[A-Z]/g, (c) => perm[c]);
        if (eval(variant)) {
            return perm;
        }
    }
    return null;
}
