class EmptyBufferException(message: String? = null, cause: Throwable? = null) : Exception(message, cause)
class BufferFullException(message: String? = null, cause: Throwable? = null) : Exception(message, cause)

class CircularBuffer<T>(capacity: Int) {
    private val data = MutableList<T?>(capacity) { null }
    private var size = 0
    private var p = 0
    private var g = 0

    val empty: Boolean
        get() = size == 0

    val full: Boolean
        get() = size == data.size

    fun read() : T {
        require(!empty) { EmptyBufferException("Buffer is empty") }

        val value = data[g]!!
        data[g] = null
        g = g.next
        --size
        return value
    }

    fun write(value: T) {
        require(!full) { BufferFullException("Buffer is full") }

        overwrite(value)
    }

    fun overwrite(value: T) {
        data[p] = value
        if (full) {
            g = g.next
        } else {
            ++size
        }
        p = p.next
    }

    fun clear() {
        data.replaceAll { null }
        size = 0
        p = 0
        g = 0
    }

    private fun require(condition: Boolean, exceptionProvider: () -> Exception) {
        if (!condition) {
            throw exceptionProvider()
        }
    }

    private val Int.next: Int
        get() = (this + 1) % data.size
}
