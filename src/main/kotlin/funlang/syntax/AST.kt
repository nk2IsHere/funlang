package funlang.syntax

inline class Name(val v: String) {
    override fun toString(): String = v
}

sealed class Expression {
    data class Double(val double: kotlin.Double) : Expression()
    data class Bool(val bool: Boolean) : Expression()
    data class String(val string: kotlin.String) : Expression()
    data class Var(val name: Name) : Expression()
    data class Let(val binder: Name, val type: Polytype?, val expr: Expression, val body: Expression) : Expression()
    data class LetRec(val binder: Name, val type: Polytype?, val expr: Expression, val body: Expression) : Expression()
    data class Constructor(val ty: Name, val dtor: Name, val fields: List<Expression>) : Expression()
    data class Match(val expr: Expression, val cases: List<Case>) : Expression()
    data class Lambda(val binder: Name, val body: Expression) : Expression() {
        fun fold(): Pair<List<Name>, Expression> {
            return when (body) {
                is Lambda -> {
                    val (innerArgs, closureBody) = body.fold()
                    listOf(binder) + innerArgs to closureBody
                }
                else -> listOf(binder) to body
            }
        }

        fun substLam(v: Name, replacement: Expression): Lambda =
            if (v == binder) this
            else this.copy(body = body.subst(v, replacement))
    }

    data class App(val function: Expression, val argument: Expression) : Expression() {
        fun unfold(): Pair<Expression, List<Expression>> =
            when (function) {
                is App -> {
                    val (func, args) = function.unfold()
                    func to args + listOf(argument)
                }
                else -> function to listOf(argument)
            }
    }

    data class If(val condition: Expression, val thenCase: Expression, val elseCase: Expression) : Expression()

    fun subst(v: Name, replacement: Expression): Expression =
        when (this) {
            is Double, is Bool, is String -> this
            is Var -> if (v == name) replacement else this
            is Lambda -> substLam(v, replacement)
            is App -> this.copy(function = function.subst(v, replacement), argument = argument.subst(v, replacement))
            is Let -> this.copy(
                expr = expr.subst(v, replacement),
                body = if (v == binder) body else body.subst(v, replacement)
            )
            is LetRec -> if (v == binder) this else this.copy(
                expr = expr.subst(v, replacement),
                body = body.subst(v, replacement)
            )
            is If -> this.copy(
                condition = condition.subst(v, replacement),
                thenCase = thenCase.subst(v, replacement),
                elseCase = elseCase.subst(v, replacement)
            )
            is Constructor -> this.copy(
                fields = fields.map { it.subst(v, replacement) }
            )
            is Match -> this.copy(
                expr = expr.subst(v, replacement),
                cases = cases.map { it.subst(v, replacement) }
            )
        }

    fun freeVars(): HashSet<Name> =
        when(this) {
            is Double, is Bool, is String -> hashSetOf()
            is Var -> hashSetOf(name)
            is Lambda -> body.freeVars().also { it.remove(binder) }
            is App -> function.freeVars().also {
                it.addAll(argument.freeVars())
            }
            is Let -> body.freeVars().also {
                it.remove(binder)
                it.addAll(expr.freeVars())
            }
            is LetRec -> expr.freeVars().also {
                it.addAll(body.freeVars())
                it.remove(binder)
            }
            is If -> condition.freeVars().also {
                it.addAll(thenCase.freeVars())
                it.addAll(elseCase.freeVars())
            }
            is Constructor -> {
                val res = hashSetOf<Name>()
                fields.forEach { res.addAll(it.freeVars()) }
                res
            }
            is Match -> expr.freeVars().also { res ->
                cases.forEach { res.addAll(it.freeVars()) }
            }
        }
}

data class Case(val pattern: Pattern, val expr: Expression) {
    fun freeVars(): HashSet<Name> {
        val res = expr.freeVars()
        res.removeAll(pattern.binders())
        return res
    }

    fun subst(v: Name, replacement: Expression): Case =
        if (pattern.binders().contains(v)) this
        else this.copy(
            expr = expr.subst(v, replacement)
        )
}

sealed class Pattern {
    data class Constructor(val ty: Name, val dtor: Name, val fields: List<Pattern>) : Pattern()
    data class Var(val v: Name) : Pattern()

    fun binders(): HashSet<Name> {
        return when (this) {
            is Constructor ->
                hashSetOf<Name>().also { res ->
                    fields.forEach { res.union(it.binders()) }
                }
            is Var -> hashSetOf(v)
        }
    }
}

inline class TyVar(val v: String) {
    override fun toString(): String = v
}

data class TypeDeclaration(
    val name: Name,
    val typeVariables: List<TyVar>,
    val dataConstructors: List<DataConstructor>
)

data class DataConstructor(val name: Name, val args: List<Monotype>)

sealed class Monotype {
    object Double : Monotype()
    object String : Monotype()
    object Bool : Monotype()
    data class Function(val argument: Monotype, val result: Monotype) : Monotype()
    data class Unknown(val u: Int) : Monotype()
    data class Var(val v: TyVar) : Monotype()
    data class Constructor(val name: Name, val arguments: List<Monotype>) : Monotype()

    fun over(f: (Monotype) -> Monotype): Monotype =
        when (this) {
            Double, String, Bool, is Unknown -> f(this)
            is Function -> f(Function(argument.over(f), result.over(f)))
            is Var -> {
                val match = Regex("""u(\d+)""").find(this.v.v)
                match?.let { Unknown(it.groupValues[1].toInt()) } ?: this
            }
            is Constructor -> f(Constructor(name, arguments.map { it.over(f) }))
        }

    private fun unknownsInner(acc: HashSet<Int>) { when (this) {
        is Constructor -> this.arguments.forEach { it.unknownsInner(acc) }
        is Var -> {
        }
        is Unknown -> acc.add(this.u)
        is Function -> {
            argument.unknownsInner(acc)
            result.unknownsInner(acc)
        }
    } }

    fun unknowns(): HashSet<Int> {
        val res = HashSet<Int>()
        unknownsInner(res)
        return res
    }

    fun subst(scrutinee: TyVar, ty: Monotype): Monotype =
        when (this) {
            is Var -> if (scrutinee == v) ty else this
            is Function -> Function(argument.subst(scrutinee, ty), result.subst(scrutinee, ty))
            is Constructor -> Constructor(name, arguments.map { it.subst(scrutinee, ty) })
            else -> this
        }

    fun substMany(tys: List<Pair<TyVar, Monotype>>): Monotype {
        return tys.fold(this) { acc, (name, ty) ->
            acc.subst(name, ty)
        }
    }

    override fun toString() = pretty()
}

data class Polytype(val vars: List<TyVar>, val type: Monotype) {
    fun unknowns(): HashSet<Int> = type.unknowns()

    companion object {
        fun fromMono(monotype: Monotype): Polytype = Polytype(emptyList(), monotype)
    }
}
