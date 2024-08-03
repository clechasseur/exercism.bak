const OPERATIONS = {
    1: (ops) => ops.push('wink'),
    2: (ops) => ops.push('double blink'),
    4: (ops) => ops.push('close your eyes'),
    8: (ops) => ops.push('jump'),
    16: (ops) => ops.reverse()
};

export function secretHandshake(input) {
    const num = Number(input);
    if (isNaN(num)) {
        throw new Error('Handshake must be a number');
    }

    const ops = [];
    for (const opBit of Object.keys(OPERATIONS)) {
        if ((num & Number(opBit)) != 0) {
            OPERATIONS[opBit](ops);
        }
    }
    return ops;
}
