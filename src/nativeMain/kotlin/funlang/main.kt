package funlang

import com.soywiz.korio.file.baseName
import com.soywiz.korio.file.baseNameWithoutExtension
import com.soywiz.korio.file.extensionLC
import com.soywiz.korio.file.std.resourcesVfs
import funlang.interpret.runProgram
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.toList
import kotlinx.coroutines.runBlocking

fun main(): Unit = runBlocking {
    resourcesVfs["test"]
        .list()
        .filter { it.isFile() && it.extensionLC == "fun" }
        .map { it.baseNameWithoutExtension.toInt() to resourcesVfs["test/${it.baseName}"].readString() } // Temp fix for korio losing abspath of file after VfsFile#list()
        .toList()
        .sortedBy { (name, _) -> name }
        .forEach { (name, program) ->
            println("Execute $name:")
            println(program)
            val result = runProgram(program, "test" to "test", "testBool" to true, "testInt" to 4)
            println("==============")
            println("Result: $result")
            println("==============")
            println()
        }
}
