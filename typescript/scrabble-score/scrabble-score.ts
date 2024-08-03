const letterValues = new Map<string, number>(Object.entries({
  "AEIOULNRST": 1,
  "DG": 2,
  "BCMP": 3,
  "FHVWY": 4,
  "K": 5,
  "JX": 8,
  "QZ": 10,
}).flatMap(([letters, score]) => [...letters].map((letter) => [letter, score])));

export function score(word: string | undefined): number {
  return [...(word || '').toUpperCase()]
    .map((letter) => letterValues.get(letter)!)
    .reduce((a, b) => a + b, 0);
}
