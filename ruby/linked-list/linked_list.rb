class Deque
  class EmptyDequeError < StandardError; end

  def push(e)
    n = Node.new(e, @tail, nil)
    @tail.next = n unless @tail.nil?
    @tail = n
    @head = n if @head.nil?
    self
  end

  def pop
    raise EmptyDequeError, "Deque is empty" if @tail.nil?
    n = @tail
    @tail = @tail.prev
    if @tail.nil?
      @head = nil
    else
      @tail.next = nil
    end
    n.value
  end

  def unshift(e)
    n = Node.new(e, nil, @head)
    @head.prev = n unless @head.nil?
    @head = n
    @tail = n if @tail.nil?
    self
  end

  def shift
    raise EmptyDequeError, "Deque is empty" if @head.nil?
    n = @head
    @head = @head.next
    if @head.nil?
      @tail = nil
    else
      @head.prev = nil
    end
    n.value
  end

  private

  Node = Struct.new(:value, :prev, :next)
end
