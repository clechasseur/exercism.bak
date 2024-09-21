class Element < Struct.new(:datum, :next); end

class SimpleLinkedList
  def initialize(enum = [])
    enum.each { push Element.new(_1) }
  end

  def push(element)
    element.next = @head
    @head = element
    self
  end

  def pop
    element = @head
    @head = @head.next unless @head.nil?
    element
  end

  def each_element
    return to_enum(:each_element) unless block_given?
    
    element = @head
    until element.nil?
      yield element
      element = element.next
    end
  end

  def to_a
    each_element.map(&:datum)
  end

  def reverse!
    elements = each_element.to_a.reverse
    elements.zip(elements[1..-1]).each { |a, b| a.next = b }
    @head = elements.first
    self
  end
end
