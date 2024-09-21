const REVERSE = {
    ']': '[',
    ')': '(',
    '}': '{',
}

export const matchingBrackets = (input) => {
    const stack = [];
    for (const c of [...input]) {
        if (/[([{]/.test(c)) {
            stack.push(c);
        } else if (stack.length === 0 || stack.pop() !== REVERSE[c]) {
            return false;
        }
    }
    return stack.length === 0;
};
