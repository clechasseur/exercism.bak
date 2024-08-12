const ALPHABET = "abcdefghijklmnopqrstuvwxyz";
const alphabetLetterAt = (pos) => ALPHABET[(pos + 26) % 26];
const randomInt = (max) => Math.floor(Math.random() * Math.floor(max));
const randomLetter = () => ALPHABET[randomInt(26)];
const randomLetters = (num) => [...Array(num).keys()].map(() => randomLetter()).join('');

export class Cipher {
    constructor(key = randomLetters(100)) {
        if (!/^[a-z]+$/.test(key)) {
            throw new Error('Bad key');
        }
        this.key = key;
    }

    encode(input) {
        return [...input].map((c, idx) => {
            return alphabetLetterAt(ALPHABET.indexOf(c) + ALPHABET.indexOf(this.key[idx % this.key.length]));
        }).join('');
    }

    decode(input) {
        return [...input].map((c, idx) => {
            return alphabetLetterAt(ALPHABET.indexOf(c) - ALPHABET.indexOf(this.key[idx % this.key.length]));
        }).join('');
    }
}
