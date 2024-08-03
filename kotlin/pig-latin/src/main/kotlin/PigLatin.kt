object PigLatin {
    fun translate(sen: String): String =
        sen.toLowerCase().replace(Regex("[a-z]+")) { translateWord(it.value) }

    private fun translateWord(word: String): String = when {
        beginsWithVowelSound(word) -> word + "ay"
        else -> moveConsonantCluster(word) + "ay"
    }

    private val vowels = "aeiou";
    private fun beginsWithVowelSound(word: String): Boolean =
        vowels.contains(word.get(0)) || word.startsWith("xr") || word.startsWith("yt")

    private val vowelsAndY = vowels + "y"
    private fun moveConsonantCluster(word: String): String {
        var indexOfVowel = word.indexOfAny(vowelsAndY.toCharArray(), if (word.get(0) == 'y') 1 else 0)
        if (indexOfVowel == -1) {
            return word
        }
        if (word.get(indexOfVowel) == 'u' && word.get(indexOfVowel - 1) == 'q') {
            indexOfVowel++
        }
        return word.substring(indexOfVowel..(word.length - 1)) + word.substring(0..(indexOfVowel - 1))
    }
}
