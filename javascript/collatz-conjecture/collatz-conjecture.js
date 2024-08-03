export function steps(n) {
    if (n <= 0) {
        throw new Error('Only positive numbers are allowed');
    }
    return n === 1 ? 0 : 1 + steps(n % 2 == 0 ? n / 2 : n * 3 + 1);
}
