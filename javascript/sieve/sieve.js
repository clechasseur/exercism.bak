function* intRange(from, to) {
  for (let i = from; i <= to; i++) {
    yield i;
  }
}

export const primes = (upTo) => {
  let candidates = [...intRange(2, upTo)];
  for (let i = 0; i < candidates.length - 1; i++) {
    const prime = candidates[i];
    for (let multiple = prime * 2; multiple <= upTo; multiple += prime) {
      const multiplePos = candidates.indexOf(multiple);
      if (multiplePos !== -1) {
        candidates.splice(multiplePos, 1);
      }
    }
  }
  return candidates;
};
