export function valid(digitString: string): boolean {
  if (/[^\d ]/.test(digitString)) {
    return false;
  }
  
  const digits: number[] = [...digitString]
      .filter((c) => c != ' ')
      .reverse()
      .map(Number)
      .map((n, i) => i % 2 != 0 ? n * 2 : n)
      .map((n) => n > 9 ? n - 9 : n);
  return digits.length > 1
    && digits.reduce((acc, n) => acc + n) % 10 == 0;
}
