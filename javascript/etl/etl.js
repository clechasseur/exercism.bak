export function transform(old) {
    const scores = [].concat(...Object.keys(old).map(score => old[score].map(letter => [letter.toLowerCase(), score])));
    const newScores = {};
    scores.forEach(([letter, score]) => newScores[letter] = Number(score));
    return newScores;
}
