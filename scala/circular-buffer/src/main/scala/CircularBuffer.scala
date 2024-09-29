class EmptyBufferException extends Exception {}
class FullBufferException extends Exception {}

class CircularBuffer(capacity: Int) {
  private val data = new Array[Int](capacity)
  private var head = 0
  private var length = 0

  private def empty = length == 0
  private def full = length == data.length
  private def incrementHead(): Unit = head = (head + 1) % data.length

  def read(): Int = {
    if (empty) {
      throw new EmptyBufferException
    }

    val result = data(head)
    incrementHead()
    length -= 1
    result
  }

  def write(value: Int): Unit = {
    if (full) {
      throw new FullBufferException
    }

    overwrite(value)
  }

  def overwrite(value: Int): Unit = {
    data((head + length) % data.length) = value
    if (full) {
      incrementHead()
    } else {
      length += 1
    }
  }

  def clear(): Unit = {
    length = 0
  }
}
