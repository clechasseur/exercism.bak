class SimpleCalculator
  ALLOWED_OPERATIONS = ['+', '/', '*'].freeze

  class UnsupportedOperation < StandardError
  end

  def self.calculate(first_operand, second_operand, operation)
    raise ArgumentError, "Wrong first operand type" unless first_operand.is_a? Integer
    raise ArgumentError, "Wrong second operand type" unless second_operand.is_a? Integer
    raise UnsupportedOperation, "Unsupported operation: #{operation}" unless ALLOWED_OPERATIONS.include? operation

    op = "#{first_operand} #{operation} #{second_operand}"
    "#{op} = #{eval op}"
  rescue ZeroDivisionError
    "Division by zero is not allowed."
  end
end
