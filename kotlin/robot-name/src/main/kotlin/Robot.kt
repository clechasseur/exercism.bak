import kotlin.random.Random

class Robot {
    companion object {
        private val names = mutableSetOf<String>()
        
        private fun newName(): String {
            var name = randomName()
            while (names.contains(name)) {
                name = randomName()
            }
            names.add(name)
            return name
        }

        private fun randomName(): String {
            val sb = StringBuilder()
            sb.append('A' + Random.nextInt(26))
            sb.append('A' + Random.nextInt(26))
            sb.append('0' + Random.nextInt(10))
            sb.append('0' + Random.nextInt(10))
            sb.append('0' + Random.nextInt(10))
            return sb.toString()
        }
    }

    var name: String = newName()
        private set
    
    fun reset() {
        name = newName()
    }
}
