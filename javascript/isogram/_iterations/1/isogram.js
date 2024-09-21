export const isIsogram = (word) => !/([a-z])\1/.test([...word.replace(/[ -]/g, '').toLowerCase()].sort().join(''));
