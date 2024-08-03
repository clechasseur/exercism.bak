export class Anagram {
  private input: string;
  private inputForAnagram: string;
  
  constructor(input: string) {
    this.input = input;
    this.inputForAnagram = Anagram.stringForAnagram(input);
  }

  public matches(...potentials: string[]): string[] {
    return potentials
      .filter((potential) => !this.matchesInput(potential))
      .filter((potential) => this.isAnagramOfInput(potential));
  }

  private matchesInput(potential: string): boolean {
    return potential.localeCompare(this.input, undefined, { sensitivity: 'accent' }) === 0;
  }

  private isAnagramOfInput(potential: string): boolean {
    return Anagram.stringForAnagram(potential) === this.inputForAnagram;
  }

  private static stringForAnagram(s: string): string {
    return s.toLocaleUpperCase().split('').sort().join('');
  }
}
