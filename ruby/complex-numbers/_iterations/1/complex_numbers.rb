class ComplexNumber
  DELTA = 0.01

  attr_reader :real, :imaginary

  def initialize(real, imaginary = 0)
    @real = real.to_f
    @imaginary = imaginary.to_f
  end

  def ==(other)
    (real - other.real).abs <= DELTA && (imaginary - other.imaginary) <= DELTA
  end

  def +(other)
    ComplexNumber.new(real + other.real, imaginary + other.imaginary)
  end

  def -(other)
    ComplexNumber.new(real - other.real, imaginary - other.imaginary)
  end

  def *(other)
    ComplexNumber.new(
      real * other.real - imaginary * other.imaginary,
      imaginary * other.real + real * other.imaginary
    )
  end

  def /(other)
    d = other.real ** 2 + other.imaginary ** 2
    ComplexNumber.new(
      (real * other.real + imaginary * other.imaginary) / d,
      (imaginary * other.real - real * other.imaginary) / d
    )
  end

  def abs
    Math.sqrt(real ** 2 + imaginary ** 2)
  end

  def conjugate
    ComplexNumber.new(real, -imaginary)
  end

  def exp
    ComplexNumber.new(Math::E ** real) * ComplexNumber.new(Math.cos(imaginary), Math.sin(imaginary))
  end
end
