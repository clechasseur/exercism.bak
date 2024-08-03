const isAnagram = (a, b) => a !== b && [...a].sort().join('') === [...b].sort().join('');

export class Anagram {
    constructor(word) {
        this.word = word.toLowerCase();
    }

    matches(candidates) {
        return candidates.filter((c) => isAnagram(this.word, c.toLowerCase()));
    }
}
