object Sieve {
    fun primesUpTo(num: Int): List<Int> = when {
        num < 2 -> emptyList()
        else -> {
            val primes = MutableList(num - 1) { i -> i + 2 }

            for (i in 2..num) {
                val prime = primes.find { it >= i } ?: break
                var factor = prime * 2
                while (factor <= num) {
                    primes.remove(factor)
                    factor += prime
                }
            }
            
            primes
        }
    }
}
