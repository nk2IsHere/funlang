package funlang.interpret

import funlang.syntax.*
import kotlinx.collections.immutable.*
import kotlin.math.sqrt

data class IREnv(private val env: PersistentMap<Name, Pair<IR, IREnvScope>> = persistentHashMapOf()) {

    enum class IREnvScope {
        GLOBAL,
        LOCAL
    }

    operator fun plus(nameToIr: Pair<Name, IR>) =
        this.copy(env = env + nameToIr.let { (name, ir) -> name to (ir to IREnvScope.GLOBAL) })

    operator fun plus(irEnv: IREnv) =
        this.copy(env = env + irEnv.env)

    operator fun plus(nameToIrs: Iterable<Pair<Name, IR>>) =
        this.copy(env = env + nameToIrs.map { (name, ir) -> name to (ir to IREnvScope.GLOBAL) })

    operator fun rem(nameToIr: Pair<Name, IR>) =
        this.copy(env = env + nameToIr.let { (name, ir) -> name to (ir to IREnvScope.LOCAL) })

    operator fun rem(nameToIrs: Iterable<Pair<Name, IR>>) =
        this.copy(env = env + nameToIrs.map { (name, ir) -> name to (ir to IREnvScope.LOCAL) })

    operator fun minus(name: Name) =
        this.copy(env = env - name)

    operator fun minus(irEnv: IREnv) =
        this.copy(env = env - irEnv.env.keys)

    operator fun minus(nameToIrs: Iterable<Pair<Name, IR>>) =
        this.copy(env = env - nameToIrs.map { it.first })

    operator fun get(name: Name) =
        this.env[name]?.let { (ir, _) -> ir }

    fun byScope(scope: IREnvScope) =
        IREnv(
            this.env.filterValues { (_, varScope) -> varScope == scope }
                .toPersistentMap()
        )

    companion object {
        fun of(vararg nameToIrs: Pair<Name, IR>, scope: IREnvScope = IREnvScope.LOCAL) =
            when(scope) {
                IREnvScope.GLOBAL -> IREnv() + listOf(*nameToIrs)
                IREnvScope.LOCAL -> IREnv() % listOf(*nameToIrs)
            }

        fun of(nameToIrs: List<Pair<Name, IR>>, scope: IREnvScope = IREnvScope.LOCAL) =
            IREnv(
                nameToIrs.associate { (name, ir) -> name to (ir to scope) }
                    .toPersistentMap()
            )
    }
}

sealed class IR {

    internal abstract fun eval(env: IREnv): IR

    sealed class IRRoot: IR() {

        abstract fun prepareEnv(env: IREnv): IREnv
    }

    interface IRPrimitiveAware {

        fun IR.matchDouble(): kotlin.Double =
            (this as? Double)?.double ?: throw Exception("Expected a Double but got $this")

        fun IR.matchBool(): Boolean =
            (this as? Bool)?.bool ?: throw Exception("Expected a Bool but got $this")

        fun IR.matchString(): kotlin.String =
            (this as? String)?.string ?: throw Exception("Expected a String but got $this")
    }

    data class Double(val double: kotlin.Double): IR() {

        override fun eval(env: IREnv): IR =
            this

        override fun toString(): kotlin.String =
            "$double"
    }

    data class Bool(val bool: Boolean): IR() {

        override fun eval(env: IREnv): IR =
            this

        override fun toString(): kotlin.String =
            "$bool"
    }

    data class String(val string: kotlin.String): IR() {

        override fun eval(env: IREnv): IR =
            this

        override fun toString(): kotlin.String =
            string
    }

    data class Map(val map: PersistentMap<IR, IR>): IR() {

        override fun eval(env: IREnv): IR =
            map[env[Name("key")]] ?: throw Exception("Unknown key in map: ${env[Name("key")]}")
    }

    data class Var(val name: Name): IR(), IRPrimitiveAware {

        override fun eval(env: IREnv): IR =
            env[name] ?: throw Exception("Unknown variable $name")
    }

    data class Lambda(val binder: Name, val body: IR, val layeredIREnv: IREnv = IREnv()) : IR() {

        override fun eval(env: IREnv): IR =
            Lambda(binder, body, env + layeredIREnv)
    }

    data class App(val function: IR, val argument: IR) : IR() {

        //TODO: use something like IRCallable to distinct between callables and others
        override fun eval(env: IREnv): IR =
            when (val closureResult = function.eval(env)) {
                is Lambda -> closureResult.body.eval(env.byScope(IREnv.IREnvScope.GLOBAL) + closureResult.layeredIREnv % (closureResult.binder to argument.eval(env)))
                else -> throw Exception("Only functions and primitives are allowed as callables, $closureResult found")
            }
    }

    data class Let(val binder: Name, val expr: IR) : IRRoot() {

        override fun prepareEnv(env: IREnv): IREnv =
            env + (binder to eval(env))

        override fun eval(env: IREnv): IR =
            expr
    }

    data class If(val condition: IR, val thenCase: IR, val elseCase: IR) : IR(), IRPrimitiveAware {

        override fun eval(env: IREnv): IR =
            when {
                condition.eval(env).matchBool() -> thenCase.eval(env)
                else -> elseCase.eval(env)
            }
    }

    data class Constructor(val ty: Name, val dtor: Name, val values: List<IR>) : IR() {

        override fun eval(env: IREnv): IR =
            Constructor(ty, dtor, values.map { it.eval(env) })
    }

    data class Match(val scrutinee: IR, val cases: List<Case>) : IR() {

        override fun eval(env: IREnv): IR {
            val scrutinee = scrutinee.eval(env) as? Constructor
                ?: throw Exception("Only type matching is supported in match construction")

            val case = cases.first { it.ty == scrutinee.ty && it.dtor == scrutinee.dtor }
            if(case.binders.size > scrutinee.values.size)
                throw Exception("case binders count can't be more than ${scrutinee.ty}::${scrutinee.dtor} can allow")

            return case.eval(env % case.binders.zip(scrutinee.values))
        }
    }

    data class When(val scrutinee: IR, val renamedScrutinee: Name, val elseCase: IR?, val conditions: List<Condition>): IR(), IRPrimitiveAware {

        override fun eval(env: IREnv): IR {
            val scrutinee = scrutinee.eval(env)
            val tmpEnv = env % (renamedScrutinee to scrutinee)

            val case = conditions.firstOrNull { it.condition.eval(tmpEnv).matchBool() }
                ?: elseCase
                ?: throw Exception("when didn't match any condition for $scrutinee")

            return case.eval(tmpEnv)
        }
    }

    data class Case(val ty: Name, val dtor: Name, val binders: List<Name>, val body: IR): IR() {

        override fun eval(env: IREnv): IR =
            body.eval(env)
    }

    data class Condition(val condition: IR, val thenCase: IR): IR() {

        override fun eval(env: IREnv): IR =
            thenCase.eval(env)
    }

    sealed class IRPrimitive(val name: Name): IRRoot() {

        override fun prepareEnv(env: IREnv): IREnv =
            env + (name to this.eval(env))

        override fun toString(): kotlin.String =
            "IRPrimitive(name=$name)"
    }

    class IRUnaryPrimitiveExecutable<T: IR>(val body: (() -> T) -> IR): IR() {

        override fun eval(env: IREnv): IR =
            body {
                (env[Name("x")] ?: throw Exception("No argument was provided")) as? T
                    ?: throw Exception("Argument is not of type required")
            }
    }

    class IRUnaryPrimitive<T: IR>(name: Name, val body: (() -> T) -> IR): IRPrimitive(name) {

        override fun eval(env: IREnv): IR =
            Lambda(
                Name("x"),
                IRUnaryPrimitiveExecutable(body)
            )
    }

    class IRBinaryPrimitiveExecutable<T: IR, R: IR>(val body: (() -> T, () -> R) -> IR): IR() {

        override fun eval(env: IREnv): IR =
            body(
                {
                    (env[Name("x")] ?: throw Exception("First argument was not provided")) as? T
                        ?: throw Exception("Argument is not of type required")
                },
                {
                    (env[Name("y")] ?: throw Exception("Second argument was not provided")) as? R
                        ?: throw Exception("Argument is not of type required")
                }
            )
    }

    class IRBinaryPrimitive<T: IR, R: IR>(name: Name, val body: (() -> T, () -> R) -> IR): IRPrimitive(name) {

        override fun eval(env: IREnv): IR =
            Lambda(
                Name("x"),
                Lambda(
                    Name("y"),
                    IRBinaryPrimitiveExecutable(body)
                )
            )
    }

    // TODO: Stub
    object IRType: IRRoot() {

        override fun prepareEnv(env: IREnv): IREnv =
            env

        override fun eval(env: IREnv): IR =
            this
    }

    data class LetMemoize(val binder: Name, val expr: Lambda) : IRRoot() {

        override fun prepareEnv(env: IREnv): IREnv =
            env + (binder to eval(env))

        override fun eval(env: IREnv): IR =
            expr
    }

    companion object {

        internal fun fromKotlinPrimitive(primitive: Any): IR = when(primitive) {
            is Number -> Double(primitive.toDouble())
            is Boolean -> Bool(primitive)
            is kotlin.String -> String(primitive)
            else -> throw Exception("Unable to convert $primitive to IR")
        }

        internal fun fromExpression(expr: Expression): IR = when (expr) {
            is Expression.Double -> Double(expr.double)
            is Expression.Bool -> Bool(expr.bool)
            is Expression.String -> String(expr.string)
            is Expression.Var -> Var(expr.name)
            is Expression.Lambda -> Lambda(expr.binder, fromExpression(expr.body))
            is Expression.App -> App(fromExpression(expr.function), fromExpression(expr.argument))
            is Expression.Let -> Let(expr.binder, fromExpression(expr.expr))
            is Expression.LetMemoize -> LetMemoize(expr.binder, fromExpression(expr.expr) as? Lambda ?: throw Exception("Only Lambda is allowed to be memoized"))
            is Expression.If -> If(fromExpression(expr.condition), fromExpression(expr.thenCase), fromExpression(expr.elseCase))
            is Expression.Constructor -> Constructor(expr.ty, expr.dtor, expr.fields.map { fromExpression(it) })
            is Expression.Match -> Match(fromExpression(expr.expr), expr.cases.map { caseFromExpr(it) })
            is Expression.When -> When(
                fromExpression(expr.field),
                expr.fieldRenamed,
                expr.elseCase?.let { fromExpression(it) },
                expr.conditions.map { conditionFromExpr(it) }
            )
            is Expression.TypeDeclaration -> IRType
        }

        private fun caseFromExpr(case: funlang.syntax.Case): Case {
            if (case.pattern !is Pattern.Constructor) error("Non constructor pattern")
            val binders = case.pattern.fields.map {
                if (it !is Pattern.Var) throw Exception("Non var pattern")
                it.v
            }

            return Case(
                case.pattern.ty,
                case.pattern.dtor,
                binders,
                fromExpression(case.expr)
            )
        }

        private fun conditionFromExpr(condition: funlang.syntax.Condition): Condition =
            Condition(
                fromExpression(condition.condition!!),
                fromExpression(condition.thenCase)
            )
    }
}

private val environmentPrimitives = persistentListOf(
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("add")) { x, y -> IR.Double(x().double + y().double) },
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("sub")) { x, y -> IR.Double(x().double - y().double) },
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("mul")) { x, y -> IR.Double(x().double * y().double) },
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("div")) { x, y -> IR.Double(x().double / y().double) },
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("mod")) { x, y -> IR.Double(x().double % y().double) },
    IR.IRUnaryPrimitive<IR.Double>(Name("sqrt")) { x -> IR.Double(sqrt(x().double)) },
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("gt")) { x, y -> IR.Bool(x().double > y().double) },
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("gte")) { x, y -> IR.Bool(x().double >= y().double) },
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("lt")) { x, y -> IR.Bool(x().double < y().double) },
    IR.IRBinaryPrimitive<IR.Double, IR.Double>(Name("lte")) { x, y -> IR.Bool(x().double <= y().double) },
    IR.IRBinaryPrimitive<IR, IR>(Name("eq")) { x, y -> IR.Bool(x() == y()) },
    IR.IRBinaryPrimitive<IR, IR>(Name("concat")) { x, y -> IR.String(x().toString() + y().toString()) },
    IR.IRUnaryPrimitive<IR>(Name("str")) { x -> IR.String(x().toString()) },
    IR.IRBinaryPrimitive<IR.Bool, IR.Bool>(Name("and")) { x, y -> IR.Bool(x().bool and y().bool) },
    IR.IRBinaryPrimitive<IR.Bool, IR.Bool>(Name("or")) { x, y -> IR.Bool(x().bool or y().bool) },
    IR.IRBinaryPrimitive<IR.Bool, IR.Bool>(Name("xor")) { x, y -> IR.Bool(x().bool xor y().bool) },
    IR.IRUnaryPrimitive<IR.Bool>(Name("not")) { x -> IR.Bool(x().bool.not()) },
    IR.IRBinaryPrimitive<IR.Map, IR>(Name("get")) { map, key -> map().eval(IREnv.of(Name("key") to key())) }
)

fun runProgram(input: String, vararg envArguments: Pair<String, Any>): IR {
    val inputPreprocessed = Preprocessor(input).process()
    val expressions = Parser.parseExpression(inputPreprocessed)

    val irEnv = (environmentPrimitives + expressions.map { (IR.fromExpression(it) as? IR.IRRoot) ?: throw Exception("$it is not IRRoot") })
        .fold(IREnv()) { acc, ir -> ir.prepareEnv(acc) }

    val mainIr = (irEnv[Name("main")] ?: throw Exception("Found no main Lambda in root")) as? IR.Lambda
        ?: throw Exception("main in root is not Lambda")

    val argsIr = IR.Map(envArguments.associate { (name, arg) -> IR.fromKotlinPrimitive(name) to IR.fromKotlinPrimitive(arg) }.toPersistentMap())

    return mainIr.body.eval(irEnv + (Name("args") to argsIr))
}
