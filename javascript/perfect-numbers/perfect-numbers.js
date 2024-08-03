export function classify(num) {
    if (num <= 0) {
        throw new Error('Classification is only possible for natural numbers.');
    }
    let aliquot = 0;
    for (let i = 1; i < num; i++) {
        if (num % i == 0) {
            aliquot += i;
        }
    }
    if (aliquot < num) {
        return 'deficient';
    } else if (aliquot > num) {
        return 'abundant';
    } else {
        return 'perfect';
    }
}
