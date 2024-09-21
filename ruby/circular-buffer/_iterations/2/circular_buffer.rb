class CircularBuffer
  class BufferEmptyException < StandardError; end
  class BufferFullException < StandardError; end

  def initialize(max_size)
    @buffer = Array.new(max_size)
    clear
  end

  def write(e)
    raise BufferFullException, "Buffer is full" if @size == @buffer.size
    @buffer[@p] = e
    @p = (@p + 1) % @buffer.size
    @size += 1
  end

  def write!(e)
    read if @size == @buffer.size
    write e
  end

  def read
    raise BufferEmptyException, "Buffer is empty" if @size == 0
    e = @buffer[@g]
    @buffer[@g] = nil
    @g = (@g + 1) % @buffer.size
    @size -= 1
    e
  end

  def clear
    @buffer.fill(nil)
    @g = @p = @size = 0
  end
end
