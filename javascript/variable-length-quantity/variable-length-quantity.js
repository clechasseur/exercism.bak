export const encode = (ints) => {
  return [].concat(...ints.map((i) => {
    const bytes = [];
    while (i > 0) {
      let byte = (i % 0x80) | 0x80;
      i = Math.floor(i / 0x80);
      bytes.unshift(byte);
    }
    if (bytes.length === 0) {
      return [0];
    } else {
      bytes[bytes.length - 1] &= 0x7f;
      return bytes;
    }
  }));
};

export const decode = (bytes) => {
  const ints = [];
  let remain = bytes.slice(0);
  while (remain.length > 0) {
    const lastByteIdx = remain.findIndex((byte) => (byte & 0x80) === 0);
    if (lastByteIdx === -1) {
      throw new Error('Incomplete sequence');
    }
    const intBytes = remain.slice(0, lastByteIdx + 1).reverse().map((byte) => byte & 0x7f);
    remain = remain.slice(lastByteIdx + 1);
    ints.push(intBytes.map((byte, idx) => byte * (0x80 ** idx)).reduce((acc, i) => acc + i));
  }
  return ints;
};
