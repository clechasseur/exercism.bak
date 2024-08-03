export function isPangram(input) {
    return [...input.toLowerCase()].filter((c) => c >= 'a' && c <= 'z')
                                   .sort()
                                   .filter((c, idx, arr) => {
                                       return idx == 0 || arr[idx - 1] !== c;
                                   })
                                   .length == 26;
}
