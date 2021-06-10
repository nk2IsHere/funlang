package funlang.syntax

class Preprocessor(private val input: String) {

    fun process() =
        input.replace(Regex("#\\*[\\S\\s]*\\*#+"), "")
            .split("\n")
            .map {
                val index = it.indexOfFirst { it == '#' }
                if(index == -1) it
                else it.subSequence(0, index)
            }
            .joinToString(separator = "\n") { it }

}
