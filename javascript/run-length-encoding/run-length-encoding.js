export function encode(str) {
    return str.replace(/([a-zA-Z ])(\1+)/g, (match, first) => `${match.length}${first}`);
}

export function decode(str) {
    return str.replace(/([0-9]+)([a-zA-Z ])/g, (_, count, char) => char.repeat(count));
}
