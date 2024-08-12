export class DnDCharacter {
  public strength: number = DnDCharacter.generateAbilityScore();
  public dexterity: number = DnDCharacter.generateAbilityScore();
  public constitution: number = DnDCharacter.generateAbilityScore();
  public intelligence: number = DnDCharacter.generateAbilityScore();
  public wisdom: number = DnDCharacter.generateAbilityScore();
  public charisma: number = DnDCharacter.generateAbilityScore();
  public hitpoints: number = 10 + DnDCharacter.getModifierFor(this.constitution);
  
  public static generateAbilityScore(): number {
    return Array.from({length: 4}, DnDCharacter.rollDice)
      .sort((a, b) => b - a)
      .slice(0, 3)
      .reduce((acc, i) => acc + i);
  }

  public static getModifierFor(abilityValue: number): number {
    return Math.floor((abilityValue - 10) / 2);
  }

  private static rollDice(): number {
    return Math.floor(Math.random() * 6) + 1;
  }
}
