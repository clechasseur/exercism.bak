class Deque<T> {
    private var head: Node<T>? = null
    private var tail: Node<T>? = null

    fun push(value: T) {
        val node = Node<T>(value)
        tail?.next = node
        node.prev = tail
        tail = node
        head = head ?: node
    }

    fun pop(): T {
        val node = tail
        tail = tail?.prev
        if (head === node) {
            head = null
        }
        return node!!.value
    }

    fun unshift(value: T) {
        val node = Node<T>(value)
        head?.prev = node
        node.next = head
        head = node
        tail = tail ?: node
    }

    fun shift(): T {
        val node = head
        head = head?.next
        if (tail === node) {
            tail = null
        }
        return node!!.value
    }
}

private class Node<T>(val value: T) {
    var prev: Node<T>? = null
    var next: Node<T>? = null
}
