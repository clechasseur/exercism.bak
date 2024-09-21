export const accumulate = (collection, predicate) => {
  const result = [];
  for (const element of collection) {
    result.push(predicate(element));
  }
  return result;
};
