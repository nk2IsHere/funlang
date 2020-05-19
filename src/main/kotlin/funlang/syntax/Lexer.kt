package funlang.syntax

import funlang.syntax.Token.*

sealed class Token {
    override fun toString(): String = this.javaClass.simpleName

    companion object {
        inline fun <reified T> get(token: Token): T? {
            return token as? T
        }
    }

    object LParen : Token()
    object RParen : Token()
    object LBrace : Token()
    object RBrace : Token()
    object LBracket : Token()
    object RBracket : Token()
    object LAngle : Token()
    object RAngle : Token()
    object Lam : Token()
    object Dot : Token()
    object Comma : Token()
    object Semicolon: Token()
    object Colon : Token()
    object DoubleColon : Token()
    object Equals: Token()
    object Arrow : Token()
    data class Ident(val ident: String) : Token()
    data class UpperIdent(val ident: String) : Token()
    data class DoubleToken(val double: Double) : Token()
    data class BoolToken(val bool: Boolean) : Token()
    data class StringToken(val string: String) : Token()
    object EOF : Token()
    object Let: Token()
    object Rec: Token()
    object In: Token()
    object Forall: Token()
    object If: Token()
    object Then: Token()
    object Else: Token()
    object Type: Token()
    object Match: Token()
    object When: Token()
    object As: Token()
    object Underscore: Token()
    object Comment: Token()
}

data class Position(val line: Int, val column: Int) {
    fun shift(n: Int) = copy(column = column + n)
    override fun toString(): String {
        return "$line:$column"
    }
}

data class Span(val start: Position, val end: Position) {
    override fun toString(): String {
        return "${this.start}-${this.end}"
    }
}
data class Spanned<out T>(val span: Span, val value: T) {
    override fun equals(other: Any?): Boolean {
        if (other is Spanned<*>) return this.value == other.value
        return false
    }

    override fun hashCode(): Int {
        return value?.hashCode() ?: 0
    }
}

class Lexer(input: String) : Iterator<Spanned<Token>> {
    var iterator = CharLocations(input.iterator())

    init {
        consumeWhitespace()
    }

    override fun hasNext(): Boolean {
        return iterator.hasNext()
    }

    override fun next(): Spanned<Token> {
        val start = iterator.position

        if (!iterator.hasNext()) {
            return Spanned(Span(start, start), EOF)
        }

        // TODO Make adequate comment handing by rewriting lexer
        if(iterator.peek() == '#') {
            iterator.next()
            while (iterator.next() != '#') { }
            consumeWhitespace()
        }

        val (token, length) = when (val c = iterator.next()) {
            '(' -> LParen to 1
            ')' -> RParen to 1
            '{' -> LBrace to 1
            '}' -> RBrace to 1
            '[' -> LBracket to 1
            ']' -> RBracket to 1
            '<' -> LAngle to 1
            '>' -> RAngle to 1
            '\\' -> Lam to 1
            '.' -> Dot to 1
            ';' -> Semicolon to 1
            ',' -> Comma to 1
            ':' -> {
                if (iterator.peek() == ':') {
                    iterator.next()
                    DoubleColon to 2
                } else {
                    Colon to 1
                }
            }
            '=' -> Equals to 1
            '-' -> when {
                iterator.peek() == '>' -> {
                    iterator.next()
                    Arrow to 2
                }
                iterator.peek().isDigit() -> doubleLiteral(c)
                else -> error("unknown construction begins with \"-\"")
            }
            '"' -> stringLiteral()
            '_' -> Underscore to 1
            else -> when {
                c.isJavaIdentifierStart() -> ident(c)
                c.isDigit() -> doubleLiteral(c)
                else -> error("$c does not correspond to any recognizable character by lexer")
            }
        }.also {
            consumeWhitespace()
        }

        return Spanned(Span(start, start.shift(length)), token)
    }

    private fun stringLiteral(): Pair<StringToken, Int> {
        var buffer = ""
        while (iterator.hasNext() && iterator.peek() != '"') {
            buffer += iterator.next()
        }
        if(!iterator.hasNext()) throw Exception("Unclosed String literal")
        iterator.next()
        return StringToken(buffer.replace("\\n", "\n")) to buffer.length + 2
    }

    private fun doubleLiteral(startChar: Char): Pair<Token, Int> {
        var result: String = startChar.toString()
        var dotCount = 0

        while (iterator.hasNext() && (iterator.peek() == '-' || iterator.peek().isDigit() || iterator.peek() == '.' && dotCount == 0)) {
            val token = iterator.next()
            if(token == '.')
                dotCount += 1

            result += token
        }
        return DoubleToken(result.toDouble()) to result.length
    }

    private fun consumeWhitespace() {
        while (iterator.hasNext() && iterator.peek().isWhitespace())
            iterator.next()
    }

    private fun ident(startChar: Char): Pair<Token, Int> {
        var result: String = startChar.toString()
        while (iterator.hasNext() && iterator.peek().isJavaIdentifierPart()) {
            result += iterator.next()
        }

        return when (result) {
            "true" -> BoolToken(true)
            "false" -> BoolToken(false)
            "let" -> Let
            "rec" -> Rec
            "in" -> In
            "if" -> If
            "then" -> Then
            "else" -> Else
            "forall" -> Forall
            "type" -> Type
            "match" -> Match
            "when" -> When
            "as" -> As
            else -> if (result[0].isUpperCase()) { UpperIdent(result) } else { Ident(result) }
        } to result.length
    }
}

class PeekableIterator<T>(private val iterator: Iterator<T>) : Iterator<T> {

    private var lookahead: T? = null

    override fun hasNext(): Boolean {
        return if (lookahead != null) {
            true
        } else {
            iterator.hasNext()
        }
    }

    override fun next(): T {
        return if (lookahead != null) {
            val temp = lookahead!!
            lookahead = null
            temp
        } else {
            iterator.next()
        }
    }

    fun peek(): T {
        lookahead = lookahead ?: iterator.next()
        return lookahead!!
    }
}

class CharLocations(private val iterator: Iterator<Char>) : Iterator<Char> {

    private var lookahead: Char? = null
    var position = Position(1, 0)

    override fun hasNext(): Boolean {
        return if (lookahead != null) {
            true
        } else {
            iterator.hasNext()
        }
    }

    override fun next(): Char {
        return if (lookahead != null) {
            val temp = lookahead!!
            lookahead = null
            nextChar(temp)
        } else
            nextChar(iterator.next())
    }

    fun peek(): Char {
        lookahead = lookahead ?: iterator.next()
        return lookahead!!
    }

    private fun nextChar(c: Char): Char {
        position = if (c == '\n') {
            Position(position.line + 1, 0)
        } else {
            Position(position.line, position.column + 1)
        }

        return c
    }
}
