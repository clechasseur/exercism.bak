enum class Unit {
    OHMS, KILOOHMS, MEGAOHMS, GIGAOHMS, TERAOHMS, PETAOHMS, EXAOHMS;

    fun next() = values()[ordinal + 1]

    override fun toString() = super.toString().toLowerCase()
}
