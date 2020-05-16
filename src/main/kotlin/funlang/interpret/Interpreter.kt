package funlang.interpret

import funlang.syntax.*
import funlang.types.*
import kotlin.math.sqrt

inline class IREnv(val env: HashMap<Name, IR> = HashMap()) {
    operator fun get(name: Name): IR? = env[name]
    fun copy(): IREnv = IREnv(HashMap(env))
    operator fun set(binder: Name, value: IR) {
        env[binder] = value
    }

    override fun toString(): String = "<env>"
}

sealed class IR {
    data class Double(val double: kotlin.Double) : IR()
    data class Bool(val bool: Boolean) : IR()
    data class String(val string: kotlin.String) : IR()
    data class Var(val name: Name) : IR()
    data class Lambda(val binder: Name, val body: IR) : IR()
    data class Closure(val binder: Name, val body: IR, val env: IREnv) : IR()
    data class App(val function: IR, val argument: IR) : IR()
    data class Let(val recursive: Boolean, val binder: Name, val expr: IR, val body: IR) : IR()
    data class If(val condition: IR, val thenCase: IR, val elseCase: IR) : IR()
    data class Constructor(val ty: Name, val dtor: Name, val values: List<IR>) : IR()
    data class Match(val scrutinee: IR, val cases: List<Case>) : IR()
    data class Case(val ty: Name, val dtor: Name, val binders: List<Name>, val body: IR): IR()

    internal fun eval(env: IREnv): IR {
        return when (this) {
            is Double, is Bool, is String, is Closure -> this
            is Var -> when {
                name.v.startsWith("#") -> evalPrim(env, name.v)
                else -> env[name] ?: throw Exception("Unknown variable $name")
            }
            is Lambda -> Closure(binder, body, env)
            is App -> when (val closure = function.eval(env)) {
                is Closure -> {
                    val tmpEnv = closure.env.copy()
                    tmpEnv[closure.binder] = argument.eval(env)
                    closure.body.eval(tmpEnv)
                }
                else -> throw Exception("$closure is not a function")
            }
            is Let -> when {
                recursive -> {
                    val tmpEnv = env.copy()
                    val closure = expr.eval(env)
                    if (closure !is Closure) throw Exception("Only functions may be declared recursively")
                    closure.env[binder] = closure
                    tmpEnv[binder] = closure
                    body.eval(tmpEnv)
                }
                else -> {
                    val tmpEnv = env.copy()
                    tmpEnv[binder] = expr.eval(env)
                    body.eval(tmpEnv)
                }
            }
            is If -> when {
                condition.eval(env).matchBool() -> thenCase.eval(env)
                else -> elseCase.eval(env)
            }
            is Constructor -> Constructor(ty, dtor, values.map { it.eval(env) })
            is Match -> {
                // TODO use Bool for this
                val scrutinee = scrutinee.eval(env) as? Constructor ?: error("Only type matching is currently supported in match construction")
                val case = cases.first { it.ty == scrutinee.ty && it.dtor == scrutinee.dtor }

                val tmpEnv = env.copy()
                if(case.binders.size > scrutinee.values.size) error("case binders count can't be more than ${scrutinee.ty}::${scrutinee.dtor} can allow")
                case.binders.forEachIndexed { index, binder -> tmpEnv[binder] = scrutinee.values[index] }

                return case.eval(tmpEnv)
            }
            is Case -> body.eval(env)
        }
    }

    private fun evalPrim(env: IREnv, prim: kotlin.String): IR {
        return when(prim) {
            "#add" -> Double(env[Name("x")]!!.matchDouble() + env[Name("y")]!!.matchDouble())
            "#sub" -> Double(env[Name("x")]!!.matchDouble() - env[Name("y")]!!.matchDouble())
            "#mul" -> Double(env[Name("x")]!!.matchDouble() * env[Name("y")]!!.matchDouble())
            "#div" -> Double(env[Name("x")]!!.matchDouble() / env[Name("y")]!!.matchDouble())
            "#mod" -> Double(env[Name("x")]!!.matchDouble() % env[Name("y")]!!.matchDouble())
            "#sqrt" -> Double(sqrt(env[Name("x")]!!.matchDouble()))
            "#gt" -> Bool(env[Name("x")]!!.matchDouble() > env[Name("y")]!!.matchDouble())
            "#gte" -> Bool(env[Name("x")]!!.matchDouble() >= env[Name("y")]!!.matchDouble())
            "#lt" -> Bool(env[Name("x")]!!.matchDouble() < env[Name("y")]!!.matchDouble())
            "#lte" -> Bool(env[Name("x")]!!.matchDouble() <= env[Name("y")]!!.matchDouble())
            "#eq" -> Bool(with(env[Name("x")]!!) {
                val x = this
                with(env[Name("y")]!!) {
                    val y = this
                    when {
                        x is Bool && y is Bool -> x.bool == y.bool
                        x is Double && y is Double -> x.double == y.double
                        x is String && y is String -> x.string == y.string
                        else -> false
                    }
                }
            })
            "#concat" -> String(env[Name("x")]!!.matchString() + env[Name("y")]!!.matchString())
            "#str" -> String(with(env[Name("x")]!!) { when (this) {
                is Double -> this.double.toString()
                is Bool -> this.bool.toString()
                is String -> this.string
                else -> this.toString()
            } })
            else -> throw Exception("Unknown primitive $prim")
        }
    }

    private fun matchDouble(): kotlin.Double =
        (this as? Double)?.double ?: throw Exception("Expected a Double but got $this")

    private fun matchBool(): Boolean =
        (this as? Bool)?.bool ?: throw Exception("Expected a Bool but got $this")

    private fun matchString(): kotlin.String =
        (this as? String)?.string ?: throw Exception("Expected a String but got $this")

    companion object {
        internal val initialEnv: IREnv = IREnv(
            hashMapOf(
                primBinary("add"),
                primBinary("sub"),
                primBinary("mul"),
                primBinary("div"),
                primBinary("mod"),
                primUnary("sqrt"),
                primBinary("gt"),
                primBinary("gte"),
                primBinary("lt"),
                primBinary("lte"),
                primBinary("eq"),
                primBinary("concat"),
                primUnary("str")
            )
        )

        private fun primUnary(prim: kotlin.String): Pair<Name, IR> =
            with(IREnv()) {
                Name(prim) to Closure(Name("x"), Var(Name("#$prim")), this)
            }

        private fun primBinary(prim: kotlin.String): Pair<Name, IR> =
            with(IREnv()) {
                Name(prim) to Closure(Name("x"), Lambda(Name("y"), Var(Name("#$prim"))), this)
            }

        internal fun fromExpr(expr: Expression, typeMap: TypeMap): IR = when (expr) {
            is Expression.Double -> Double(expr.double)
            is Expression.Bool -> Bool(expr.bool)
            is Expression.String -> String(expr.string)
            is Expression.Var -> Var(expr.name)
            is Expression.Lambda -> Lambda(expr.binder, fromExpr(expr.body, typeMap))
            is Expression.App -> App(fromExpr(expr.function, typeMap), fromExpr(expr.argument, typeMap))
            is Expression.Let -> Let(false, expr.binder, fromExpr(expr.expr, typeMap), fromExpr(expr.body, typeMap))
            is Expression.LetRec -> Let(true, expr.binder, fromExpr(expr.expr, typeMap), fromExpr(expr.body, typeMap))
            is Expression.If -> If(fromExpr(expr.condition, typeMap), fromExpr(expr.thenCase, typeMap), fromExpr(expr.elseCase, typeMap))
            is Expression.Constructor -> Constructor(expr.ty, expr.dtor, expr.fields.map { fromExpr(it, typeMap) })
            is Expression.Match -> Match(fromExpr(expr.expr, typeMap), expr.cases.map { lowerCase(it, typeMap) })
        }

        private fun lowerCase(case: funlang.syntax.Case, typeMap: TypeMap): Case {
            if (case.pattern !is Pattern.Constructor) error("Non constructor pattern")

            val binders = case.pattern.fields.map {
                if (it !is Pattern.Var) throw Exception("Non var pattern")
                it.v
            }

            return Case(
                case.pattern.ty,
                case.pattern.dtor,
                binders,
                fromExpr(case.expr, typeMap)
            )
        }

        private fun tagForDtor(type: Name, dtor: Name, typeMap: TypeMap): Int {
            val tyInfo = typeMap.tm[type] ?: throw Exception("Unknown type name $type")
            val ix = tyInfo.constructors.indexOfFirst { dtor == it.name }
            return if (ix != -1) ix else throw Exception("Unknown dtor name $dtor")
        }
    }
}

class TypeInferrer {
    private val initialEnv =  listOf(
        "add" to "Double -> Double -> Double",
        "sub" to "Double -> Double -> Double",
        "mul" to "Double -> Double -> Double",
        "div" to "Double -> Double -> Double",
        "mod" to "Double -> Double -> Double",
        "sqrt" to "Double -> Double",
        "gt" to "Double -> Double -> Bool",
        "gte" to "Double -> Double -> Bool",
        "lt" to "Double -> Double -> Bool",
        "lte" to "Double -> Double -> Bool",
        "eq" to "a -> b -> Bool",
        "str" to "a -> String",
        "concat" to "String -> String -> String"
    ).fold(Environment()) { acc, (name, ty) ->
        acc.extend(Name(name), Parser.parseType(ty))
    }

    fun infer(expr: Expression, typeMap: TypeMap) =
        TypeChecker(CheckState.initial()).withTypeMap(typeMap)
            .inferExpr(initialEnv, expr)
}

fun runProgram(input: String): Pair<Monotype, IR> {
    val (types, expr) = Parser.parseExpression(input)
    val typeMap = TypeMap(HashMap())
    types.forEach { typeMap.tm[it.name] = TypeInfo(it.typeVariables, it.dataConstructors) }

    val type = TypeInferrer().infer(expr, typeMap)
    val ir = IR.fromExpr(expr, typeMap)
    return type to ir.eval(IR.initialEnv)
}
