function isPrime(n) {
  for (let f = 2; f <= Math.floor(Math.sqrt(n)); f++) {
    if (n % f === 0) {
      return false;
    }
  }
  return true;
}

export class DiffieHellman {
  constructor(p, g) {
    if (p < 2 || g < 2 || !isPrime(p) || !isPrime(g)) {
      throw new Error('Invalid input');
    }
    this.p = p;
    this.g = g;
  }

  getPublicKeyFromPrivateKey(privKey) {
    if (privKey <= 1 || privKey >= this.p) {
      throw new Error('Invalid private key');
    }
    return (this.g ** privKey) % this.p;
  }

  getSharedSecret(privKey, pubKey) {
    return (pubKey ** privKey) % this.p;
  }
}
