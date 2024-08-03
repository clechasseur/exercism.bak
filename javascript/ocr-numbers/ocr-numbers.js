const DIGITS = [
    [' _ ',
     '| |',
     '|_|'],
    ['   ',
     '  |',
     '  |'],
    [' _ ',
     ' _|',
     '|_ '],
    [' _ ',
     ' _|',
     ' _|'],
    ['   ',
     '|_|',
     '  |'],
    [' _ ',
     '|_ ',
     ' _|'],
    [' _ ',
     '|_ ',
     '|_|'],
    [' _ ',
     '  |',
     '  |'],
    [' _ ',
     '|_|',
     '|_|'],
    [' _ ',
     '|_|',
     ' _|']
].map((a) => a.join(''));

function convertDigit(digit) {
    const digitIdx = DIGITS.indexOf(digit.join(''));
    return digitIdx != -1 ? digitIdx.toString() : '?';
}

export function convert(input) {
    const output = [];
    const lines = input.split('\n');
    let topLine = 0;
    while (lines.length > topLine) {
        if (topLine != 0) {
            output.push(',');
        }
        let leftColumn = 0;
        while (lines[topLine].length > leftColumn) {
            output.push(convertDigit([
                lines[topLine].slice(leftColumn, leftColumn + 3),
                lines[topLine + 1].slice(leftColumn, leftColumn + 3),
                lines[topLine + 2].slice(leftColumn, leftColumn + 3)
            ]));
            leftColumn += 3;
        }
        topLine += 4;
    }
    return output.join('');
}
