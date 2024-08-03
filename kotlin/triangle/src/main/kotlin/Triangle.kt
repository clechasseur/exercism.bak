class Triangle(val side1: Double, val side2: Double, val side3: Double) {
    constructor(side1: Int, side2: Int, side3: Int)
        : this(side1.toDouble(), side2.toDouble(), side3.toDouble())

    val sides = listOf(side1, side2, side3)
    val equalSides: Int by lazy {
        sides.toSet().size
    }
    
    init {
        require(sides.all { it > 0.0 })
        require(sides.mapIndexed {
            idx, len -> sides.filterIndexed { inidx, _ -> inidx != idx }.sum() >= len
        }.all { it })
    }

    val isEquilateral get() = equalSides == 1
    val isIsosceles get() = equalSides <= 2
    val isScalene get() = equalSides == 3
}
