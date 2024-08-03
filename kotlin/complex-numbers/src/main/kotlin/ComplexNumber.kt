import kotlin.math.*

data class ComplexNumber(val real: Double = 0.0, val imag: Double = 0.0) {
    val abs: Double
        get() = sqrt(real.pow(2) + imag.pow(2))

    operator fun plus(z: ComplexNumber) = ComplexNumber(real + z.real, imag + z.imag)
    operator fun minus(z: ComplexNumber) = ComplexNumber(real - z.real, imag - z.imag)

    operator fun times(z: ComplexNumber) = ComplexNumber(
        real = (real * z.real) - (imag * z.imag),
        imag = (imag * z.real) + (real * z.imag)
    )
    operator fun div(z: ComplexNumber) = ComplexNumber(
        real = ((real * z.real) + (imag * z.imag)) / (z.real.pow(2) + z.imag.pow(2)),
        imag = ((imag * z.real) - (real * z.imag)) / (z.real.pow(2) + z.imag.pow(2))
    )

    fun conjugate() = ComplexNumber(real, -imag)
}

fun exponential(z: ComplexNumber): ComplexNumber
    = ComplexNumber(real = exp(z.real)) * ComplexNumber(cos(z.imag), sin(z.imag))
