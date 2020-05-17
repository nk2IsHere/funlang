package funlang.types

import funlang.syntax.*
import kotlin.Exception
import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf

data class Substitution(val subst: HashMap<String, Monotype>) {
    fun apply(ty: Monotype): Monotype =
        when (ty) {
            is Monotype.Unknown -> subst["u${ty.u}"]?.let(::apply) ?: ty
            is Monotype.Var -> subst[ty.v.v]?.let(::apply) ?: ty
            is Monotype.Function -> Monotype.Function(apply(ty.argument), apply(ty.result))
            is Monotype.Constructor -> ty.copy(arguments = ty.arguments.map { apply(it) })
            else -> ty
        }

    override fun toString(): String =
        "{ " + subst.toList().joinToString("\n, ") { (u, ty) -> "u$u ↦ ${ty.pretty()}" } + "\n}"
}

data class Environment(val env: PersistentMap<Name, Polytype> = persistentHashMapOf()) {
    operator fun get(name: Name): Polytype? = env[name]

    fun unknowns(): HashSet<Int> {
        val res = HashSet<Int>()
        for ((_, ty) in env) {
            res.union(ty.unknowns())
        }
        return res
    }

    fun extend(name: Name, type: Polytype): Environment = Environment(env.put(name, type))
    fun extend(names: List<Pair<Name, Polytype>>): Environment = Environment(env.putAll(names.toMap()))
    fun extendMono(names: List<Pair<Name, Monotype>>): Environment = extend(names.map { (name, ty) -> name to Polytype.fromMono(ty)})
    fun extendMono(name: Name, type: Monotype): Environment = extend(name, Polytype.fromMono(type))
}

data class TypeInfo(val tyArgs: List<TyVar>, val constructors: List<DataConstructor>) {
    companion object {
        val empty = TypeInfo(listOf(), listOf())
    }
}

data class TypeMap(val tm: HashMap<Name, TypeInfo>)

data class CheckState(
    var typeMap: TypeMap,
    val substitution: Substitution = Substitution(HashMap()),
    var freshSupply: Int = 0
) {
    companion object {
        fun initial(): CheckState =
            CheckState(TypeMap(
                hashMapOf(
                    Name("Double") to TypeInfo.empty,
                    Name("Bool") to TypeInfo.empty,
                    Name("String") to TypeInfo.empty
                )
            ))
    }
}

class TypeChecker(var checkState: CheckState) {

    // Returns a fresh `Unknown`, where fresh means "not ever used before"
    private fun freshUnknown(): Monotype = Monotype.Unknown(++checkState.freshSupply)

    // Applies the current substitution to a given type
    fun zonk(ty: Monotype): Monotype = checkState.substitution.apply(ty)

    private fun instantiate(ty: Polytype): Monotype {
        var result = ty.type
        for (v in ty.vars) {
            result = result.subst(v, freshUnknown())
        }
        return result
    }

    private fun lookupName(env: Environment, v: Name): Monotype =
        env[v]?.let(::instantiate) ?: throw Exception("Unknown variable $v")

    private fun lookupType(ty: Name): TypeInfo = checkState.typeMap.tm[ty] ?: throw Exception("Unknown type $ty")

    private fun lookupDataConstructor(ty: Name, dtor: Name): Pair<List<TyVar>, List<Monotype>> {
        val tyInfo = lookupType(ty)
        val dataConstructor = tyInfo.constructors.find { it.name == dtor }
            ?: throw Exception("Unknown dtor $ty::$dtor")
        return tyInfo.tyArgs to dataConstructor.args
    }

//    private fun generalise(env: Environment, ty: Monotype): Polytype {
//        val ty = zonk(ty)
//        val niceVars = "abcdefghijklmnopqrstuvwxyz".iterator()
//        val quantified: MutableList<TyVar> = mutableListOf()
//        val subst: HashMap<Int, Monotype> = HashMap()
//        val envUnknowns = env.unknowns()
//        for (free in ty.unknowns()) {
//            if (!envUnknowns.contains(free)) {
//                val tyVar = TyVar(niceVars.nextChar().toString())
//                quantified.add(tyVar)
//                subst[free] = Monotype.Var(tyVar)
//            }
//        }
//        return Polytype(quantified, Substitution(subst).apply(ty))
//    }

    private fun solveType(u: Int, ty: Monotype) {
        if (ty.unknowns().contains(u)) error("u${u} is already present somewhere before")
        checkState.substitution.subst["u${u}"] = ty
    }

    private fun solveType(v: String, ty: Monotype) {
        if (ty.vars().contains(v)) error("$v is already present somewhere before")
        checkState.substitution.subst[v] = ty
    }

    fun withTypeMap(typeMap: TypeMap): TypeChecker {
        checkState.typeMap.tm.putAll(typeMap.tm)
        return this
    }

    fun addType(tyDecl: TypeDeclaration) {
        checkState.typeMap.tm[tyDecl.name] = TypeInfo(tyDecl.typeVariables, tyDecl.dataConstructors)
    }

    fun unify(ty1: Monotype, ty2: Monotype) {
        val ty1 = zonk(ty1)
        val ty2 = zonk(ty2)

        println("$ty1 $ty2")

        if (ty1 != ty2) {
            when {
                ty1 is Monotype.Constructor && ty2 is Monotype.Constructor -> {
                    if (ty1.name != ty2.name) throw UnifyException(ty1, ty2, mutableListOf())
                    try {
                        ty1.arguments.zip(ty2.arguments) { t1, t2 ->
                            unify(t1, t2)
                        }
                    } catch (ex: UnifyException) {
                        ex.stack.add(ty1 to ty2)
                        throw ex
                    }
                }
                ty1 is Monotype.Unknown -> solveType(ty1.u, ty2)
                ty2 is Monotype.Unknown -> solveType(ty2.u, ty1)
                ty1 is Monotype.Var -> solveType(ty1.v.v, ty2)
                ty2 is Monotype.Var -> solveType(ty2.v.v, ty1)
                ty1 is Monotype.Function && ty2 is Monotype.Function -> {
                    try {
                        unify(ty1.argument, ty2.argument)
                        unify(ty1.result, ty2.result)
                    } catch (ex: UnifyException) {
                        ex.stack.add(ty1 to ty2)
                        throw ex
                    }
                }
                ty2 is Monotype.Function -> {
                    unify(ty1, ty2.argument)
                }
                else -> {
                    println("unify $ty1 $ty2")
                    throw UnifyException(ty1, ty2, mutableListOf())
                }
            }
        }
    }

    private fun subsumes(ty1: Polytype, ty2: Polytype) {
        // TODO higher-rank types
        unify(instantiate(ty1), ty2.type)
    }

    private fun inferPattern(pattern: Pattern, ty: Monotype): List<Pair<Name, Monotype>> {
        return when (pattern) {
            is Pattern.Constructor -> {
                val (tyArgs, fields) = lookupDataConstructor(pattern.ty, pattern.dtor)
                val freshVars = tyArgs.map { it to freshUnknown() }
                unify(ty, Monotype.Constructor(pattern.ty, freshVars.map { it.second }))
                pattern.fields.zip(fields).flatMap { (pat, ty) -> inferPattern(pat, ty.substMany(freshVars)) }
            }
            is Pattern.Var -> listOf(pattern.v to ty)
        }
    }

    private fun infer(env: Environment, expr: Expression): Monotype =
        when (expr) {
            is Expression.Double -> Monotype.Double
            is Expression.Bool -> Monotype.Bool
            is Expression.String -> Monotype.String
            is Expression.Var -> lookupName(env, expr.name)
            is Expression.Let -> {
                var tyBinder = Polytype.fromMono(infer(env, expr.expr))
                if (expr.type != null) {
                    subsumes(tyBinder, expr.type)
                    tyBinder = expr.type
                }
                infer (env.extend(expr.binder, tyBinder), expr.body)
            }
            is Expression.LetRec -> {
                val envBinder = expr.type ?: Polytype.fromMono(freshUnknown())
                var tyBinder = Polytype.fromMono(infer(env.extend(expr.binder, envBinder), expr.expr))

                if (expr.type != null) {
                    subsumes(tyBinder, expr.type)
                    tyBinder = expr.type
                }
                infer(env.extend(expr.binder, tyBinder), expr.body)
            }
            is Expression.Lambda -> {
                val tyBinder = freshUnknown()
                val tyBody = infer(env.extendMono(expr.binder, tyBinder), expr.body)
                Monotype.Function(tyBinder, tyBody)
            }
            is Expression.App -> {
                val tyResult = freshUnknown()
                val tyFun = infer(env, expr.function)
                val tyArg = infer(env, expr.argument)
                unify(tyFun, Monotype.Function(tyArg, tyResult))
                tyResult
            }
            is Expression.If -> {
                val tyCond = infer(env, expr.condition)
                unify(tyCond, Monotype.Bool)
                val tyThen = infer(env,expr.thenCase)
                val tyElse = infer(env, expr.elseCase)
                unify(tyThen, tyElse)
                tyThen // Could just as well be tyElse
            }
            is Expression.Match -> {
                val tyExpr = infer(env, expr.expr)
                val tyRes = freshUnknown()
                expr.cases.forEach {
                    val typedNames = inferPattern(it.pattern, tyExpr)
                    val tyCase = infer(env.extendMono(typedNames), it.expr)
                    unify(tyRes, tyCase)
                }
                tyRes
            }
            is Expression.Constructor -> {
                val (tyArgs, fields) = lookupDataConstructor(expr.ty, expr.dtor)
                val freshVars = tyArgs.map { it to freshUnknown() }
                expr.fields.zip(fields).forEach { (expr, ty) ->
                    unify(ty.substMany(freshVars), infer(env, expr))
                }
                Monotype.Constructor(expr.ty, freshVars.map { it.second })
            }
        }

    fun inferExpr(env: Environment, expr: Expression): Monotype = zonk(infer(env, expr))
}

data class UnifyException(val ty1: Monotype, val ty2: Monotype, val stack: MutableList<Pair<Monotype, Monotype>>) :
    Exception() {
    override fun toString(): String {
        return """
            Failed to match ${ty1.pretty()} with ${ty2.pretty()}
            ${stack.joinToString("\n  ") { (t1, t2) -> "while trying to match ${t1.pretty()} with ${t2.pretty()}" }}
        """.trimIndent()
    }
}

