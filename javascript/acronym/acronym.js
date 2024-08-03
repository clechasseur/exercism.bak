export const parse = (phrase) => {
    const acronym = [];
    const re = /([a-z'])[a-z']*/gi;
    let match;
    while ((match = re.exec(phrase)) !== null) {
        acronym.push(match[1].toUpperCase());
    }
    return acronym.join('');
};
