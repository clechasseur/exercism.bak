class BinarySearchTree<T : Comparable<T>> {

    data class Node<T : Comparable<T>>(val data: T) {
        var left: Node<T>? = null
            private set

        var right: Node<T>? = null
            private set

        var parent: Node<T>? = null
            private set

        fun insert(node: Node<T>) {
            when {
                node.data <= data -> insertLeft(node)
                else -> insertRight(node)
            }
        }

        private fun insertLeft(node: Node<T>) {
            when (left) {
                null -> {
                    left = node
                    node.parent = this
                }
                else -> left?.insert(node)
            }
        }

        private fun insertRight(node: Node<T>) {
            when (right) {
                null -> {
                    right = node
                    node.parent = this
                }
                else -> right?.insert(node)
            }
        }
    }

    var root: Node<T>? = null
        private set

    fun insert(value: T) {
        val node = Node(value)
        when (root) {
            null -> root = node
            else -> root?.insert(node)
        }
    }

    fun asSortedList(): List<T> = NodeIterable(root).map { it.second }

    fun asLevelOrderList(): List<T> = NodeIterable(root).sortedBy { it.first }.map { it.second }

    private class NodeIterable<T : Comparable<T>>(private val root: Node<T>?) : Iterable<Pair<Int, T>> {
        override fun iterator(): Iterator<Pair<Int, T>> = NodeIterator(root)
    }

    private class NodeIterator<T : Comparable<T>>(root: Node<T>?) : Iterator<Pair<Int, T>> {
        private var node = root
        private var level = 0

        init {
            drillLeft()
        }

        override fun hasNext() = node != null

        override fun next(): Pair<Int, T> {
            val result = level to node!!.data
            moveNext()
            return result
        }

        private fun drillLeft() {
            while (node?.left != null) {
                node = node?.left
                level++
            }
        }

        private fun moveNext() {
            if (node!!.right == null) {
                while (node?.parent != null && node?.parent?.right == node) {
                    node = node?.parent
                    level--
                }
                node = node?.parent
                level--
            } else {
                node = node?.right
                level++
                drillLeft()
            }
        }
    }
}
