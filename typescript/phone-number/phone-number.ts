export function clean(input: string): string {
  let numbers: string = input.replaceAll(/[\s()-.+]+/g, '');

  if (/\p{Letter}/u.test(numbers)) {
    throw new Error('Letters not permitted');
  } else if (/\D/.test(numbers)) {
    throw new Error('Punctuations not permitted');
  } else if (numbers.length < 10) {
    throw new Error('Incorrect number of digits');
  } else if (numbers.length > 11) {
    throw new Error('More than 11 digits');
  }

  if (numbers.length === 11) {
    if (numbers[0] !== '1') {
      throw new Error('11 digits must start with 1');
    }
    numbers = numbers.substring(1);
  }

  checkDigit(numbers, 0, 'Area code');
  checkDigit(numbers, 3, 'Exchange code');

  return numbers;
}

function checkDigit(numbers: string, digitPos: number, digitKind: string) {
  const digit = numbers[digitPos];

  if (digit === '0') {
    throw new Error(`${digitKind} cannot start with zero`);
  } else if (digit === '1') {
    throw new Error(`${digitKind} cannot start with one`);
  }
}
