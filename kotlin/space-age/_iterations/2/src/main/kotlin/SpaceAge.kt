import kotlin.math.round

class SpaceAge(val ageInEarthSeconds: Long) {
    fun onEarth() = onPlanet(1.0)
    fun onMercury() = onPlanet(0.2408467)
    fun onVenus() = onPlanet(0.61519726)
    fun onMars() = onPlanet(1.8808158)
    fun onJupiter() = onPlanet(11.862615)
    fun onSaturn() = onPlanet(29.447498)
    fun onUranus() = onPlanet(84.016846)
    fun onNeptune() = onPlanet(164.79132)

    private fun onPlanet(orbitalPeriod: Double): Double
        = round((ageInEarthSeconds.toDouble() / 31_557_600) / orbitalPeriod * 100.0) / 100.0
}
