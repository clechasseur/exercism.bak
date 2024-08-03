class Reactor<T : Comparable<T>>() {
    interface Subscription {
        fun cancel()
    }

    interface Cell<T : Comparable<T>> {
        var value: T
    }

    inner class InputCell(initialValue: T) : Cell<T> {
        override var value: T = initialValue
            set(newValue) {
                if (newValue != field) {
                    field = newValue
                    this@Reactor.recompute(this)
                }
            }
    }

    inner class ComputeCell(vararg val dependsOn: Cell<T>, val computeCallback: (List<T>) -> T) : Cell<T> {
        init {
            this@Reactor.computeCells.add(this)
        }

        private var _value: T = computeValue()
        private val subs = mutableListOf<ComputeSubscription>()

        override var value: T
            get() = _value
            set(_) {
                throw UnsupportedOperationException("Cannot set value of compute cell")
            }

        fun addCallback(callback: (T) -> Unit): Subscription {
            val sub = ComputeSubscription(callback)
            subs.add(sub)
            return sub
        }

        internal fun update() {
            val newValue = computeValue()
            if (newValue != _value) {
                _value = newValue
                subs.forEach { it.call() }
            }
        }

        private fun computeValue() = computeCallback(dependsOn.map { it.value })

        private inner class ComputeSubscription(val callback: (T) -> Unit) : Subscription {
            override fun cancel() {
                this@ComputeCell.subs.remove(this)
            }

            fun call() {
                callback(this@ComputeCell.value)
            }
        }
    }

    private val computeCells = mutableListOf<ComputeCell>()

    private fun recompute(initial: InputCell) {
        val plan = mutableListOf<ComputeCell>()
        for (cell in computeCells) {
            if (cell.dependsOn.any { it in plan || it === initial }) {
                plan.add(cell)
            }
        }
        plan.forEach { it.update() }
    }
}
