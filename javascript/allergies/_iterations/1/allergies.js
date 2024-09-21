const ALLERGIES = {
    1: 'eggs',
    2: 'peanuts',
    4: 'shellfish',
    8: 'strawberries',
    16: 'tomatoes',
    32: 'chocolate',
    64: 'pollen',
    128: 'cats'
};

export class Allergies {
    constructor(score) {
        this.allergies = Object.keys(ALLERGIES).filter(is => (score & Number(is)) !== 0)
                                               .map(is => ALLERGIES[is]);
    }

    list() {
        return this.allergies;
    }

    allergicTo(allergen) {
        return this.allergies.indexOf(allergen) != -1;
    }
}
