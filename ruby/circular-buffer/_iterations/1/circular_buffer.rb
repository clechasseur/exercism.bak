class CircularBuffer
  class BufferEmptyException < StandardError; end
  class BufferFullException < StandardError; end

  def initialize(max_size)
    @max_size = max_size
    clear
  end

  def write(e)
    raise BufferFullException, "Buffer is full" if @size == @buffer.size
    write! e
  end

  def write!(e)
    @buffer[@p] = e
    @g = (@g + 1) % @buffer.size if @size == @buffer.size
    @p = (@p + 1) % @buffer.size
    @size += 1 if @size < @buffer.size
  end

  def read
    raise BufferEmptyException, "Buffer is empty" if @size == 0
    e = @buffer[@g]
    @g = (@g + 1) % @buffer.size
    @size -= 1
    e
  end

  def clear
    @buffer = Array.new(@max_size)
    @g = @p = @size = 0
  end
end
