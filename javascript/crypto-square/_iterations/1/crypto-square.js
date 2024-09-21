export class Crypto {
    constructor(input) {
        this.normalized = input.toLowerCase().replace(/[^a-zA-Z0-9]/g, '');
        this.columns = Math.ceil(Math.sqrt(this.normalized.length));
        this.rows = this.columns;
        if ((this.columns ** 2) - this.normalized.length >= this.columns) {
            this.rows -= 1;
        }
        this.segments = [...Array(this.rows).keys()].map((rowIdx) => this.normalized.slice(rowIdx * this.columns, (rowIdx + 1) * this.columns));
        this.cipher = [...Array(this.columns).keys()].map((colIdx) => this.segments.map((row) => colIdx < row.length ? row[colIdx] : '').join('')).join('');
    }

    normalizePlaintext() {
        return this.normalized;
    }

    size() {
        return this.columns;
    }

    plaintextSegments() {
        return this.segments;
    }

    ciphertext() {
        return this.cipher;
    }
}
