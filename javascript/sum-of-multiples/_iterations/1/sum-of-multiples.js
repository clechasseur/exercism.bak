function* intRange(from, to) {
  for (let i = from; i <= to; i++) {
    yield i;
  }
}

export const sumOfMultiples = (multiplesOf, upTo) => {
  return [...intRange(2, upTo - 1)].filter((i) => multiplesOf.some((mult) => i % mult === 0)).reduce((acc, i) => acc + i, 0);
};
