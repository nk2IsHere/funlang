package funlang.syntax

import pretty.*
import pretty.symbols.*

fun Expression.show(): Doc<Nothing> = showInner(0)
fun Expression.pretty(): kotlin.String {
    return show().pretty(60, 0.4F)
}

fun Expression.showInner(depth: Int): Doc<Nothing> = when (this) {
    is Expression.Double -> Double.toString().text()
    is Expression.Bool -> bool.toString().text()
    is Expression.String -> string.toString().text()
    is Expression.Var -> name.show()
    is Expression.Lambda -> {
        var res = (("\\".text() + binder.show() + dot()).group() + space() + body.show()).group().nest(2)
        if (depth > 0) res = res.enclose(lParen(), rParen())
        res
    }
    is Expression.App -> {
        val (func, args) = unfold()
        var res = ((listOf(func) + args).map { it.showInner(1) }).sep().group().nest(2)
        if (depth > 0) res = res.enclose(lParen(), rParen())
        res
    }
    is Expression.Let ->
        "let".text() + space() + binder.show() + space() + equals() + space() +
                expr.show() + space() + "in".text() + line() +
                body.show()
    is Expression.If -> (("if".text() + space() + condition.show() + space() + "then".text()).group() +
            space() + thenCase.show()).group().nest(2) +
            space() + ("else".text() + space() + elseCase.show()).group().nest(2)
    is Expression.LetRec -> "letrec".text() + space() + binder.show() + space() + equals() + space() +
        expr.show() + space() + "in".text() + line() +
        body.show()
    is Expression.Constructor -> {
        val dt = ty.show() + "::".text() + dtor.show() + lParen()
        val fs = fields.fold(nil()) { acc, it -> acc + it.show() + comma() + softLine() }
        dt + fs.nest(2) + rParen()
    }
    is Expression.Match -> {
        val header = "match".text() + space() + expr.show() + space() + lBrace()
        val showCase = { case: Case ->
            case.pattern.show() + space() + "=>".text() + space() + (line() +
                case.expr.show()).nest(2)
        }
        val myCases = cases.fold(nil()) { acc, it -> acc + showCase(it) + comma() + line() }
        header.group() + (line() + myCases).nest(2) + rBrace()
    }
    is Expression.When -> {
        val header = "when".text() + space() + field.show() + space() + "as".text() + space() + fieldRenamed.show() + lBrace()
        val showCondition = { condition: Condition ->
            (condition.condition?.show() ?: "_".text()) + space() + "=>".text() + space() + (line() +
                condition.thenCase.show()).nest(2)
        }
        val myConditions = conditions.fold(nil()) { acc, it -> acc + showCondition(it) + comma() + line() }
        header.group() + (line() + myConditions).nest(2) + rBrace()
    }
}

private fun Monotype.prettyInner(nested: Boolean): kotlin.String {
    return when (this) {
        is Monotype.Unknown -> "u${this.u}"
        is Monotype.Function -> {
            val res = "${this.argument.prettyInner(true)} -> ${this.result.pretty()}"
            if(nested) "($res)" else res
        }
        Monotype.Any -> "Any"
        Monotype.Double -> "Double"
        Monotype.String -> "String"
        Monotype.Bool -> "Bool"
        is Monotype.Var -> this.v.v
        is Monotype.Constructor ->
            "${this.name}" + (
                 if (arguments.isNotEmpty())
                    "<${this.arguments.joinToString(", ") { it.pretty() }}>"
                 else ""
             )
    }
}

fun Monotype.pretty(): String = prettyInner(false)
fun Name.show(): Doc<Nothing> = v.text()
fun Pattern.show(): Doc<Nothing> {
    return when (this) {
        is Pattern.Constructor -> ty.show() + "::".text() + dtor.show() + fields.map { it.show() }.encloseSep(
            lParen(),
            rParen(),
            comma()
        )
        is Pattern.Var -> v.show()
    }
}
fun Case.pretty(): String = "${pattern.pretty()} => ${expr.pretty()}"
fun Pattern.pretty(): String = when (this) {
    is Pattern.Constructor -> "$ty::$dtor(${fields.joinToString(", ") { it.pretty() }})"
    is Pattern.Var -> v.toString()
}
fun Polytype.pretty(): String = if (vars.isEmpty()) {
    type.pretty()
} else {
    "forall ${vars.joinToString()}. ${type.pretty()}"
}
fun Condition.pretty(): String = "${condition?.pretty() ?: "_"} => ${thenCase.pretty()}"
