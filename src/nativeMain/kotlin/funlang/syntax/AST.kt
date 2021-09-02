package funlang.syntax

import kotlinx.serialization.Serializable

@Serializable
value class Name(val v: String) {
    override fun toString(): String = v
}

sealed class Expression {
    data class Double(val double: kotlin.Double) : Expression()
    data class Bool(val bool: Boolean) : Expression()
    data class String(val string: kotlin.String) : Expression()
    data class Var(val name: Name) : Expression()
    data class Let(val binder: Name, val type: Polytype?, val expr: Expression) : Expression()
    data class LetMemoize(val binder: Name, val type: Polytype?, val expr: Expression) : Expression()
    data class Constructor(val ty: Name, val dtor: Name, val fields: List<Expression>) : Expression()
    data class Match(val expr: Expression, val cases: List<Case>) : Expression()
    data class Lambda(val binder: Name, val body: Expression) : Expression()

    data class App(val function: Expression, val argument: Expression) : Expression()

    data class If(val condition: Expression, val thenCase: Expression, val elseCase: Expression) : Expression()
    data class When(val field: Expression, val fieldRenamed: Name, val elseCase: Expression?, val conditions: List<Condition>) : Expression()

    data class TypeDeclaration(
        val name: Name,
        val typeVariables: List<TyVar>,
        val dataConstructors: List<DataConstructor>
    ): Expression()
}

data class Case(val pattern: Pattern, val expr: Expression)

data class Condition(val condition: Expression?, val thenCase: Expression) // null is accepted in parsing as else branch

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

value class TyVar(val v: String) {
    override fun toString(): String = v
}

data class DataConstructor(val name: Name, val args: List<Monotype>)

sealed class Monotype {
    object Any: Monotype()
    object Double: Monotype()
    object String: Monotype()
    object Bool : Monotype()
    data class Function(val argument: Monotype, val result: Monotype) : Monotype()
    data class Unknown(val u: Int) : Monotype()
    data class Var(val v: TyVar) : Monotype()
    data class Constructor(val name: Name, val arguments: List<Monotype>) : Monotype()

    fun over(f: (Monotype) -> Monotype): Monotype =
        when (this) {
            Any, Double, String, Bool, is Unknown -> f(this)
            is Function -> f(Function(argument.over(f), result.over(f)))
            is Var -> {
                val match = Regex("""u(\d+)""").find(this.v.v)
                match?.let { Unknown(it.groupValues[1].toInt()) } ?: this
            }
            is Constructor -> f(Constructor(name, arguments.map { it.over(f) }))
        }

    private fun unknownsInner(acc: HashSet<Int>) { when (this) {
        is Constructor -> this.arguments.forEach { it.unknownsInner(acc) }
        is Var -> {}
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

    private fun varsInner(acc: HashSet<kotlin.String>) { when (this) {
        is Constructor -> this.arguments.forEach { it.varsInner(acc) }
        is Var -> acc.add(this.v.v)
        is Function -> {
            argument.varsInner(acc)
            result.varsInner(acc)
        }
    } }

    fun vars(): HashSet<kotlin.String> {
        val res = HashSet<kotlin.String>()
        varsInner(res)
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
}

data class Polytype(val vars: List<TyVar>, val type: Monotype) {
    fun unknowns(): HashSet<Int> = type.unknowns()

    companion object {
        fun fromMono(monotype: Monotype): Polytype = Polytype(emptyList(), monotype)
    }
}
