export const encode = (input) => {
    return [...input.replace(/[^a-zA-Z0-9]/g, '').toLowerCase()].map((c) => {
        return (c >= 'a' && c <= 'z') ? String.fromCharCode('z'.charCodeAt() - (c.charCodeAt() - 'a'.charCodeAt())) : c;
    }).join('').replace(/.{1,5}/g, '$& ').slice(0, -1);
}
