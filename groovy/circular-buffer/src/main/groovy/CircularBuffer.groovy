class EmptyBufferException extends Exception {}
class FullBufferException extends Exception {}

class CircularBuffer {
    private List<Integer> data
    private int head = 0
    private int size = 0

    CircularBuffer(int capacity) {
        data = [0] * capacity
    }

    def isEmpty() {
        size == 0
    }

    def isFull() {
        size == data.size()
    }

    def clear() {
        size = 0
    }

    def read() {
        if (isEmpty()) {
            throw new EmptyBufferException()
        }

        def result = data[head]
        incrementHead()
        --size
        result
    }

    def write(int item) {
        if (isFull()) {
            throw new FullBufferException()
        }

        overwrite(item)
    }

    def overwrite(int item) {
        def pos = (head + size) % data.size()
        data[pos] = item
        if (isFull()) {
            incrementHead()
        } else {
            ++size
        }
    }

    private def incrementHead() {
        head = (head + 1) % data.size()
    }
}
