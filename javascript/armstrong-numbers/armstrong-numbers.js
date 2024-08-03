export const validate = (input) => Array.from('' + input).map((c, _, arr) => parseInt(c, 10) ** arr.length).reduce((prev, cur) => prev + cur, 0) == input;
