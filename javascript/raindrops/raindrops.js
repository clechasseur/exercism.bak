const SOUNDS = ['Pling', 'Plang', 'Plong'];
export const convert = (input) => [3, 5, 7].map((i, idx) => input % i == 0 ? SOUNDS[idx] : '').join('') || ('' + input);
