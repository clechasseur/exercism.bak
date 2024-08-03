function* intRange(min, max) {
    for (let i = min; i <= max; i++) {
        yield i;
    }
}

export function transpose(rows) {
    const transposedRow = (idx) => rows.map((row) => {
        const rowChars = [...row];
        return rowChars.length > idx ? rowChars[idx] : ' ';
    }).join('');
    const longestRow = rows.reduce((prev, row) => row.length > prev ? row.length : prev, 0);
    const transposedRows = [...intRange(0, longestRow - 1)].map((i) => transposedRow(i));
    for (let i = transposedRows.length - 1; i >= 0; i--) {
        if (!transposedRows[i].endsWith(' ')) {
            break;
        }
        transposedRows[i] = transposedRows[i].trimEnd();
    }
    return transposedRows;
}
