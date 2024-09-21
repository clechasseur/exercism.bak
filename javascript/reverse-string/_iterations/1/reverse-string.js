export const reverseString = (input) => [...Array(input.length).keys()].reverse().map(idx => input[idx]).join('');
