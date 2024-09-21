const randomDice = () => Math.floor(Math.random() * 6) + 1;

export function abilityModifier(ability) {
    if (ability < 3) {
        throw new Error('Ability scores must be at least 3');
    } else if (ability > 18) {
        throw new Error('Ability scores can be at most 18');
    }
    return Math.floor((ability - 10) / 2);
}

export class Character {
    constructor() {
        this.strength = Character.rollAbility();
        this.dexterity = Character.rollAbility();
        this.constitution = Character.rollAbility();
        this.intelligence = Character.rollAbility();
        this.wisdom = Character.rollAbility();
        this.charisma = Character.rollAbility();
        this.hitpoints = 10 + abilityModifier(this.constitution);
    }

    static rollAbility() {
        return [randomDice(), randomDice(), randomDice(), randomDice()].sort((a, b) => b - a).slice(0, 3).reduce((prev, cur) => prev + cur);
    }
}
