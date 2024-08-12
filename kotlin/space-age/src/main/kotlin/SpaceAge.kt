import java.math.BigDecimal

class SpaceAge(private val ageInSeconds: Long) {

    private val NUMBER_OF_SECONDS_IN_A_YEAR = 31557600L

    private val ORBITAL_PERIOD = mapOf(
        "MERCURY" to 0.2408467,
        "VENUS" to 0.61519726,
        "MARS" to 1.8808158,
        "JUPITER" to 11.862615,
        "SATURN" to 29.447498,
        "URANUS" to 84.016846,
        "NEPTUNE" to 164.79132
    )

    private val HUNDREDTH = 2

    private fun calculateAgeOn(planet: String): Double =
        (ageInSeconds.toDouble() / (NUMBER_OF_SECONDS_IN_A_YEAR * ORBITAL_PERIOD.getValue(planet)))
            .toBigDecimal()
            .setScale(HUNDREDTH, BigDecimal.ROUND_HALF_UP)
            .toDouble()

    fun onEarth(): Double = calculateAgeOn("EARTH")

    fun onMercury(): Double = calculateAgeOn("MERCURY")

    fun onVenus(): Double = calculateAgeOn("VENUS")

    fun onMars(): Double = calculateAgeOn("MARS")

    fun onJupiter(): Double = calculateAgeOn("JUPITER")

    fun onSaturn(): Double = calculateAgeOn("SATURN")

    fun onUranus(): Double = calculateAgeOn("URANUS")

    fun onNeptune(): Double = calculateAgeOn("NEPTUNE")
}