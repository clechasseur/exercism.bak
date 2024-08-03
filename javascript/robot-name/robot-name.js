const ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const usedNames = new Set();
const randomInt = (max) => Math.floor(Math.random() * Math.floor(max));
const randomLetter = () => ALPHABET[randomInt(26)];
const randomName = () => `${randomLetter()}${randomLetter()}${('' + randomInt(1000)).padStart(3, '0')}`;
function uniqueRandomName() {
    let name;
    do {
        name = randomName();
    } while (usedNames.has(name));
    usedNames.add(name);
    return name;
}


export class Robot {
    constructor() {
        this.reset();
    }

    get name() {
        return this._name;
    }
    set name(_) {
        throw new Error('Cannot set internal name');
    }
    reset() {
        this._name = uniqueRandomName();
    }

    static releaseNames() {
        usedNames.clear();
    }
}
