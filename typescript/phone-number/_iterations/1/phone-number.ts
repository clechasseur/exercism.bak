export function clean(input: string): string {
  if (/[A-Za-z]/.test(input)) {
    throw new Error('Letters not permitted');
  } else if (!/^\s*(\+?\d)?\s*\(?\s*\d+\s*\)?[\s-.]*\d+[\s-.]*\d+[\s-.]*\d+\s*$/.test(input)) {
    throw new Error('Punctuations not permitted');
  }

  let numbers: string = input.replaceAll(/[^\d]+/g, '');

  if (numbers.length < 10) {
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

  const areaCode = numbers[0];
  if (areaCode === '0') {
    throw new Error('Area code cannot start with zero');
  } else if (areaCode === '1') {
    throw new Error('Area code cannot start with one');
  }
  
  const exchangeCode = numbers[3];
  if (exchangeCode === '0') {
    throw new Error('Exchange code cannot start with zero');
  } else if (exchangeCode === '1') {
    throw new Error('Exchange code cannot start with one');
  }

  return numbers;
}
