import java.util.concurrent.atomic.AtomicInteger

class BankAccount {
    private val _balance: AtomicInteger = AtomicInteger()

    val balance: Int get() {
        val curBalance = _balance.get()
        checkClosed(curBalance)
        return curBalance
    }

    fun adjustBalance(delta: Int) {
        _balance.accumulateAndGet(delta) {
            cur, del ->
                checkClosed(cur)
                if (cur + del < 0) {
                    throw IllegalStateException("Account balance ($cur) does not have enough for withdrawal (${-del})")
                }
                cur + del
        }
    }

    fun close() {
        _balance.set(-1)
    }

    private fun checkClosed(curBalance: Int) {
        if (curBalance == -1) {
            throw IllegalStateException("Account is closed")
        }
    }
}
