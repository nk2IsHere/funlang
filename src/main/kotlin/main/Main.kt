package main

import funlang.interpret.runProgram

fun main() {
//    val (type, result) = runProgram("""
//        let rec fib =
//            \x.
//                if eq x 1 then
//                    1
//                else if eq x 2 then
//                    1
//                else
//                    add (fib (sub x 1)) (fib (sub x 2))
//        in
//
//        fib 20
//    """.trimIndent())
//
//    val (type, result) = runProgram("""
//        type List<a> { Val(a, List<a>), Nil() }
//
//        let rec map: forall a b. (a -> b) -> List<a> -> List<b> =
//            \f. \ls.
//                match ls {
//                    List::Nil() -> List::Nil(),
//                    List::Val(x, xs) -> List::Val(f x, map f xs),
//                }
//        in
//
//        let fizzbuzz: Double -> String =
//            \x.
//                if eq (mod x 15) 0 then
//                    "FizzBuzz"
//                else if eq (mod x 3) 0 then
//                    "Fizz"
//                else if eq (mod x 3) 0 then
//                    "Buzz"
//                else
//                    str x
//        in
//
//        map (\x. fizzbuzz x)  List::Val(1, List::Val(2, List::Val(3, List::Nil()))))
//    """.trimIndent())

//    val (type, result) = runProgram("""
//        concat args_test (concat (str args_testBool) (concat (str args_testInt) "!!")))
//    """.trimIndent(), "test" to "KEK!", "testInt" to 4, "testBool" to false)

    val (type, result) = runProgram("""
        # i am a comment #
        (\a. when a as xs {
            eq xs 5 -> "55",
            and (gt xs 10) (lt xs 100) -> "100",
            lt xs 200 -> "200",
            _ -> "1000", # me too! #
        }) 231
    """.trimIndent())
    println("type: $type\nresult: $result")
}
