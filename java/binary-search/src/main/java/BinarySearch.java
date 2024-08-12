import java.util.List;

class BinarySearch<T extends Comparable<T>> {
    private final List<T> elements;

    BinarySearch(List<T> elements) {
        this.elements = elements;
    }

    int indexOf(T value) throws ValueNotFoundException {
        return indexOf(value, 0, elements.size());
    }

    private int indexOf(T value, int start, int end) throws ValueNotFoundException {
        if (start == end) {
            throw new ValueNotFoundException("Value not in array");
        }
        int pos = start + ((end - start) / 2);
        if (elements.get(pos).equals(value)) {
            return pos;
        }
        int cmp = elements.get(pos).compareTo(value);
        if (cmp < 0) {
            return indexOf(value, pos + 1, end);
        } else {
            return indexOf(value, start, pos);
        }
    }
}
