class PhoneNumber(val input: String) {
    companion object {
        private val NANP_NUMBER_FORMAT = Regex("[2-9][0-9]{2}[2-9][0-9]{6}")
    }

    val number: String? by lazy {
        val cleanInput = input.filter { it.isDigit() }
                              .filterIndexed { idx, c -> idx != 0 || c != '1' }
        if (cleanInput.matches(NANP_NUMBER_FORMAT)) cleanInput else null
    }
}
