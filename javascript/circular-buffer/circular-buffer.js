class CircularBuffer {
    constructor(size) {
        this.buffer = Array(size);
        this.clear();
    }

    read() {
        if (this.length === 0) {
            throw new BufferEmptyError();
        }
        const element = this.buffer[this.readpoint];
        this.readpoint = this.wrapInc(this.readpoint);
        this.length--;
        return element;
    }

    write(element) {
        if (element && this.length === this.buffer.length) {
            throw new BufferFullError();
        }
        this.forceWrite(element);
    }

    forceWrite(element) {
        if (!element) {
            return;
        }
        this.buffer[this.writepoint] = element;
        this.writepoint = this.wrapInc(this.writepoint);
        if (this.length === this.buffer.length) {
            this.readpoint = this.writepoint;
        } else {
            this.length++;
        }
    }

    clear() {
        this.writepoint = 0;
        this.readpoint = 0;
        this.length = 0;
    }

    wrapInc(num) {
        return (num + 1) === this.buffer.length ? 0 : num + 1;
    }
}

function circularBuffer(size) {
    return new CircularBuffer(size);
}

export class BufferFullError extends Error { }
export class BufferEmptyError extends Error { }

export default circularBuffer;
