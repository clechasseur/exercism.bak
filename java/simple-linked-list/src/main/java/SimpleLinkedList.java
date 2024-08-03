import java.lang.reflect.Array;
import java.util.NoSuchElementException;

class SimpleLinkedList<T> {
    private Element<T> head;
    private Element<T> tail;
    private int siz = 0;

    SimpleLinkedList() {
    }

    @SafeVarargs
    SimpleLinkedList(T... values) {
        for (T value : values) {
            push(value);
        }
    }

    int size() {
        return siz;
    }

    void push(T value) {
        Element<T> element = new Element<>(value);
        element.next = head;
        head = element;
        if (tail == null) {
            tail = element;
        }
        siz++;
    }

    T pop() {
        if (siz == 0) {
            throw new NoSuchElementException("Cannot pop from an empty list");
        }
        Element<T> element = head;
        head = head.next;
        if (head == null) {
            tail = null;
        }
        siz--;
        return element.value;
    }

    void reverse() {
        Element<T> element = head;
        head = tail = null;
        siz = 0;
        while (element != null) {
            push(element.value);
            element = element.next;
        }
    }

    T[] asArray(Class<T> clazz) {
        @SuppressWarnings("unchecked")
        T[] output = (T[]) Array.newInstance(clazz, siz);
        Element<T> element = head;
        int i = 0;
        while (element != null) {
            output[i] = element.value;
            element = element.next;
            i++;
        }
        return output;
    }

    private static final class Element<T> {
        final T value;
        Element<T> next;

        Element(T value) {
            this.value = value;
        }
    }
}