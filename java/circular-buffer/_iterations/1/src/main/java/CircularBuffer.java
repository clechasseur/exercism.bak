import java.util.ArrayList;
import java.util.List;

class CircularBuffer<T> {
    private List<T> data;
    private int g = 0;
    private int p = 0;
    private int siz = 0;

    CircularBuffer(int capacity) {
        data = new ArrayList<>();
        for (int i = 0; i < capacity; i++) {
            data.add(null);
        }
    }

    T read() throws BufferIOException {
        if (siz == 0) {
            throw new BufferIOException("Tried to read from empty buffer");
        }
        siz--;
        T value = data.get(g);
        data.set(incg(), null);
        return value;
    }

    void write(T value) throws BufferIOException {
        if (siz == data.size()) {
            throw new BufferIOException("Tried to write to full buffer");
        }
        overwrite(value);
    }

    void overwrite(T value) {
        data.set(incp(), value);
        if (siz < data.size()) {
            siz++;
        } else {
            incg();
        }
    }

    void clear() {
        g = p = siz = 0;
    }

    private int incg() {
        int value = g++;
        if (g >= data.size()) {
            g = 0;
        }
        return value;
    }

    private int incp() {
        int value = p++;
        if (p >= data.size()) {
            p = 0;
        }
        return value;
    }
}