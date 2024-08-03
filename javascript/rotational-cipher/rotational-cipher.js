export class RotationalCipher {
  static rotate(message, key) {
    return [...message].map((c) => RotationalCipher.rotateOne(c, key)).join('');
  }

  static rotateOne(c, key) {
    if (c >= 'a' && c <= 'z') {
      return RotationalCipher.rotateOneFrom(c, 'a', key);
    } else if (c >= 'A' && c <= 'Z') {
      return RotationalCipher.rotateOneFrom(c, 'A', key);
    }
    return c;
  }

  static rotateOneFrom(c, first, key) {
    const firstCode = first.charCodeAt();
    let newCCode = c.charCodeAt() + key;
    while (newCCode < firstCode) {
      newCCode += 26;
    }
    while (newCCode >= (firstCode + 26)) {
      newCCode -= 26;
    }
    return String.fromCharCode(newCCode);
  }
}
