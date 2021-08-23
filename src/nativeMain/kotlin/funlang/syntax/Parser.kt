package funlang.syntax

import funlang.syntax.Token.Companion.get

class Parser(tokens: Iterator<Spanned<Token>>) {

    val iterator = PeekableIterator(tokens)

    fun parseInput(): List<Expression> {
        val expressions = mutableListOf<Expression>()
        while(iterator.hasNext()) {
            if(iterator.peek().value !in listOf(Token.Type, Token.Let)) {
                throw ParserException(
                    expressions.last().toString(),
                    "Only let and type are expected as root tokens, " +
                        "found: ${iterator.peek().value}, " +
                        "please check if parentheses are closed correctly")
            }
            expressions.add(parseExpression(root = true))
        }

        return expressions
    }

    private fun parsePolytype(): Polytype {
        val vars = mutableListOf<TyVar>()
        val t = iterator.peek()
        if (t.value is Token.Forall) {
            iterator.next()
            while (true) {
                vars += parseTyVar()
                if (iterator.peek().value == Token.Dot) {
                    iterator.next()
                    break
                }
            }
        }
        val ty = parseType()

        return Polytype(vars, ty)
    }

    private fun parseTypeDeclaration(): Expression.TypeDeclaration {
        expectNext<Token.Type>(expectedError("expected type"))
        val name = parseUpperName()

        expectNext<Token.LAngle>(expectedError("expected open brace"))
        val typeVariables = commaSeparated(::parseTyVar) { it !is Token.RAngle }
        expectNext<Token.RAngle>(expectedError("expected closing brace"))

        expectNext<Token.LBrace>(expectedError("expected open brace"))
        val dataConstructors = commaSeparated(::parseDataConstructor) { it !is Token.RBrace }
        expectNext<Token.RBrace>(expectedError("expected closing brace"))

        return Expression.TypeDeclaration(name, typeVariables, dataConstructors)
    }

    private fun parseDataConstructor(): DataConstructor {
        val name = parseUpperName()
        expectNext<Token.LParen>(expectedError("expected left paren"))
        val fields = commaSeparated(::parseType) { it !is Token.RParen }
        expectNext<Token.RParen>(expectedError("expected right paren"))

        return DataConstructor(name, fields)
    }


    private fun parseType(): Monotype {
        val arg = parseTypeAtom()
        return when (iterator.peek().value) {
            is Token.Arrow -> {
                iterator.next()
                val res = parseType()
                Monotype.Function(arg, res)
            }
            else -> arg
        }
    }

    private fun parseTypeAtom(): Monotype {
        val (start, nextToken) = iterator.peek()

        return when (nextToken) {
            is Token.LParen -> {
                iterator.next()

                val type = parseType()
                expectNext<Token.RParen>(expectedError("missing closing paren"))
                type
            }
            is Token.Ident -> {
                val tyVar = parseTyVar()
                val match = Regex("""u(\d+)""").find(tyVar.v)
                match?.let { Monotype.Unknown(it.groupValues[1].toInt()) }
                    ?: Monotype.Var(tyVar)
            }
            is Token.UpperIdent -> {
                val name = parseUpperName()
                var args = emptyList<Monotype>()
                if (iterator.peek().value is Token.LAngle) {
                    expectNext<Token.LAngle>(expectedError("expected open brace"))
                    args = commaSeparated(::parseType) { it !is Token.RAngle }
                    expectNext<Token.RAngle>(expectedError("expected closing brace"))
                }
                when (name.v) {
                    "Double" -> Monotype.Double
                    "String" -> Monotype.String
                    "Bool" -> Monotype.Bool
                    "Any" -> Monotype.Any
                    else -> Monotype.Constructor(name, args)
                }
            }
            else -> throw RuntimeException("expected type found $nextToken at $start")
        }
    }

    private fun parseName(): Name {
        val ident = expectNext<Token.Ident>(expectedError("expected identifier"))
        return Name(ident.value.ident)
    }

    private fun parseUpperName(): Name {
        val ident = expectNext<Token.UpperIdent>(expectedError("expected uppercase identifier"))
        return Name(ident.value.ident)
    }

    private fun parseTyVar(): TyVar {
        val ident = expectNext<Token.Ident>(expectedError("expected identifier"))
        return TyVar(ident.value.ident)
    }

    private fun parseVar(): Expression.Var = Expression.Var(parseName())


    private fun parseDouble(): Expression.Double {
        val (_, doubleToken) = expectNext<Token.DoubleToken>(expectedError("expected double literal"))
        return Expression.Double(doubleToken.double)
    }

    private fun parseBool(): Expression.Bool {
        val (_, boolToken) = expectNext<Token.BoolToken>(expectedError("expected boolean literal"))
        return Expression.Bool(boolToken.bool)
    }

    private fun parseString(): Expression.String {
        val (_, stringToken) = expectNext<Token.StringToken>(expectedError("expected string literal"))
        return Expression.String(stringToken.string)
    }


    private fun parseLambda(): Expression.Lambda {
        iterator.next()

        val binder = parseName()
        expectNext<Token.Dot>(expectedError("expected dot"))

        val body = parseExpression()

        return Expression.Lambda(binder, body)
    }

    private fun parseExpression(root: Boolean = false): Expression {
        val atoms = mutableListOf<Expression>()
        while (iterator.hasNext()) {
            // FIXME: dirty hack to stop parser from including Let in non-roots and parsing more than one root at call
            if(!root && iterator.peek().value == Token.Let
                || root && atoms.isNotEmpty())
                break

            parseAtom()?.let { atoms += it }
                ?: break
        }

        return when {
            atoms.isEmpty() -> throw RuntimeException("failed to parse expression")
            atoms.size == 1 -> atoms.first()
            else -> atoms.drop(1).fold(atoms[0]) { func, arg ->
                Expression.App(func, arg)
            }
        }

    }

    private fun parseAtom(): Expression? {
        return when (iterator.peek().value) {
            is Token.LParen -> {
                iterator.next()
                val expr = parseExpression()
                expectNext<Token.RParen>(expectedError("missing closing paren"))

                expr
            }
            is Token.Lam -> parseLambda()
            is Token.Ident -> parseVar()
            is Token.DoubleToken -> parseDouble()
            is Token.BoolToken -> parseBool()
            is Token.StringToken -> parseString()
            is Token.Let -> parseLet()
            is Token.If -> parseIf()
            is Token.UpperIdent -> parseDataConstruction()
            is Token.Match -> parseMatch()
            is Token.When -> parseWhen()
            is Token.Type -> parseTypeDeclaration()
            else -> null
        }
    }

    private fun parseLet(): Expression {
        iterator.next()
        var isRecursive = false
        if (iterator.peek().value is Token.Memoize) {
            iterator.next()
            isRecursive = true
        }
        val binder = parseName()
        var type: Polytype? = null
        if (iterator.peek().value is Token.Colon) {
            iterator.next()
            type = parsePolytype()
        }
        expectNext<Token.Equals>(expectedError("expected equals"))
        val expr = parseExpression()

        return if (isRecursive) {
            Expression.LetMemoize(binder, type, expr)
        } else {
            Expression.Let(binder, type, expr)
        }
    }

    private fun parseIf(): Expression.If { // if true then 3 else 4
        iterator.next()
        val condition = parseExpression()
        expectNext<Token.Then>(expectedError("expected then"))
        val thenBranch = parseExpression()
        expectNext<Token.Else>(expectedError("expected else"))
        val elseBranch = parseExpression()

        return Expression.If(condition, thenBranch, elseBranch)
    }

    private fun parseDataConstruction(): Expression.Constructor {
        val type = parseUpperName()
        expectNext<Token.DoubleColon>(expectedError("expected double colon"))
        val dtor = parseUpperName()
        expectNext<Token.LParen>(expectedError("expected open paren"))

        val exprs = commaSeparated(::parseExpression) { it !is Token.RParen }
        expectNext<Token.RParen>(expectedError("expected closing paren"))

        return Expression.Constructor(type, dtor, exprs)
    }

    private fun parseMatch(): Expression.Match {
        iterator.next()
        val expr = parseExpression()
        expectNext<Token.LBrace>(expectedError("expected open brace"))
        val cases = commaSeparated(::parseCase) { it !is Token.RBrace }
        expectNext<Token.RBrace>(expectedError("expected closing brace"))
        return Expression.Match(expr, cases)
    }

    private fun parseCase(): Case {
        val pattern = parsePattern()
        expectNext<Token.Arrow>(expectedError("expected arrow"))
        val body = parseExpression()

        return Case(pattern, body)
    }

    private fun parseWhen(): Expression.When {
        iterator.next()
        val field = parseExpression()
        expectNext<Token.As>(expectedError("expected as"))
        val fieldRenamed = parseName()
        expectNext<Token.LBrace>(expectedError("expected open brace"))
        val conditions = commaSeparated(::parseCondition) { it !is Token.RBrace }
        expectNext<Token.RBrace>(expectedError("expected closing brace"))

        if(conditions.isEmpty()) error(expectedError("expected at least one condition"))
        return Expression.When(
            field,
            fieldRenamed,
            conditions.firstOrNull { it.condition == null }?.thenCase,
            conditions.filter { it.condition != null }
        )
    }

    private fun parseCondition(): Condition {
        val condition = if(iterator.peek().value !is Token.Underscore) parseExpression() else null.also { iterator.next() }
        expectNext<Token.Arrow>(expectedError("expected arrow"))
        val thenCase = parseExpression()

        return Condition(condition, thenCase)
    }

    private fun parsePattern(): Pattern {
        return if (iterator.peek().value is Token.Ident) {
            Pattern.Var(parseName())
        } else {
            val ty = parseUpperName()
            expectNext<Token.DoubleColon>(expectedError("expect double colon"))
            val dtor = parseUpperName()
            expectNext<Token.LParen>(expectedError("expected open paren"))
            val fields = commaSeparated(::parsePattern) { it !is Token.RParen }
            expectNext<Token.RParen>(expectedError("expected open paren"))
            Pattern.Constructor(ty, dtor, fields)
        }
    }

    private fun expectedError(msg: String): (Spanned<Token>) -> String = { token ->
        "$msg saw ${token.value} at ${token.span}"
    }

    private inline fun <reified T> expectNext(tokenToError: (token: Spanned<Token>) -> String): Spanned<T> {
        val (span, token) = iterator.next()

        get<T>(token)?.let { return Spanned(span, it) }
            ?: error(tokenToError(Spanned(span, token)))
    }

    private fun <T> commaSeparated(parser: () -> T, cont: (Token) -> Boolean): List<T> {
        val result = mutableListOf<T>()
        while (cont(iterator.peek().value)) {
            result += parser()

            if (iterator.peek().value == Token.Comma) iterator.next()
            else break
        }
        return result
    }

    companion object {
        fun parseExpression(input: String): List<Expression> = Parser(Lexer(input)).parseInput()
    }
}

class ParserException(expression: String, message: String): RuntimeException("""
      An exception has been thrown while parsing
      $expression
      Message: $message
""".trimIndent())
