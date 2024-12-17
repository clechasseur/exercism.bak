const digitValueOf0 = '0'.charCodeAt(0);
const digitValue = (digit) => digit.charCodeAt(0) - digitValueOf0;

export const isArmstrongNumber = (input) =>
  [...String(input)]
    .map((d, _, arr) => digitValue(d) ** arr.length)
    .reduce((prev, cur) => prev + cur, 0) === input;
