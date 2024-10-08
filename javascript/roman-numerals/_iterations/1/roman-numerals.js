const NUMERALS = [
    [1000, 'M'],
    [ 900, 'CM'],
    [ 500, 'D'],
    [ 400, 'CD'],
    [ 100, 'C'],
    [  90, 'XC'],
    [  50, 'L'],
    [  40, 'XL'],
    [  10, 'X'],
    [   9, 'IX'],
    [   5, 'V'],
    [   4, 'IV'],
    [   1, 'I']
];

export const toRoman = (num) => {
    let asRoman = '';
    let remain = num;
    NUMERALS.forEach(([int, roman]) => {
        while (remain >= int) {
            asRoman += roman;
            remain -= int;
        }
    });
    return asRoman;
}
