export class Prime {
  nth(n) {
    if (n < 1) {
      throw new Error('Prime is not possible');
    }
    let i = 1;
    while (n > 0) {
      i++;
      if (this.isPrime(i)) {
        n--;
      }
    }
    return i;
  }

  isPrime(i) {
    for (let factor = 2; factor <= Math.floor(Math.sqrt(i)); factor++) {
      if (i % factor === 0) {
        return false;
      }
    }
    return true;
  }
}
