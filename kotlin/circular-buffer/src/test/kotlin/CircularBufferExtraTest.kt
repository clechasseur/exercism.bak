import kotlin.test.Test
import kotlin.test.assertEquals

class CircularBufferExtraTest {
    @Test
    fun `works with nullable types`() {
        val buffer = CircularBuffer<Int?>(1)

        buffer.write(1)
        assertEquals(1, buffer.read())

        buffer.write(null)
        assertEquals(null, buffer.read())
    }
}
