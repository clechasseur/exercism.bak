export function primeFactors(n) {
    let remain = n;
    let factor = 2;
    const factors = [];
    while (remain > 1) {
        if (remain % factor == 0) {
            factors.push(factor);
            remain /= factor;
        } else {
            factor++;
        }
    }
    return factors;
}