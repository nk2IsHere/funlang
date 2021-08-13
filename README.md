# funlang
Functional language written on Kotlin
> The funlang.main idea behind its developement is to create a language which will quickly compute or handle any required logic with potential jvm interop without any side effects and nulls!

### Keypoints:
- 4 basic types: Double (64-bit IEEE), Bool, String, Lambda, Let (Named anything)
- Each expression must return something
- Every basic thing, like adding is a function
- There is no (and there will be no) input or output using system streams - [Deprecated, very lazy to do that]

### How to run program:
> Simply import `funlang.interpret.runProgram` and call `runProgram`\
  Result will be returned

### Standard functions:
`add 2 5` - adds two doubles\
`sub 5 2` - subtracts two doubles\
`mul 5 24` - multiplies two doubles\
`div 2 434` - divides two doubles\
`mod 10 3` - calculates modulus using two doubles\
`sqrt 10` - calculates square rectangle from double\
`gt(or gte, lt, lte) 3 3` - `>`, `>=`, `<`, `<=` using two doubles\
`concat "a" "!"` - adds two strings\
`str 2.13` - converts type to string\
`and(or, xor) true false` - bool operations using two doubles\
`not true` = false!


### TODO:
- Write tests (😂👌👌👌👌)
- Rewrite lexer (it's just bad)
- Add examples on types, match and where constructions
- Add modules (separate files with types and its specific functions)
- Redo type system
- Add Dicts (and Lists (Dict<Double, Any>))

### Examples:
#### Double (basically any number is double)
```
let main =
  \args.
    5.0
```

#### Bool
```
let main =
  \args.
    false
```

#### String
```
let main =
  \args.
    "I am string!"
```

#### If
```
let main =
  \args.
    if gt 2 3 then
        "2 > 3"
    else 
        "You guessed it!"
```

#### Lambda
```
let main =
  \args.
    (\x. x) "Lambda can be called instantly like this!"
```

#### Let
```
let bangIt = 
    \x. 
        concat x "!" 

let main =
  \args.
    bangIt "Hello and don't forget to try stacking multiple let's :D"
```

#### Higher-ordered Lambdas
```
let flip = 
    \f. \x. \y. f y x 

let const = 
    \x. \y. x 

let main =
  \args.
    flip const "Henlo!" 2.4
```

#### Fibonacci the bad way
```
let rec fib =
    \x.
        if eq x 1 then
            1
        else if eq x 2 then
            1
        else
            add (fib (sub x 1)) (fib (sub x 2))

let main =
  \args.
    fib 20
```

### Deprecated from here (will redo some day later):

#### FizzBuzz with List type and Match!
```
type List<a> { Val(a, List<a>), Nil() }

let rec map: forall a b. (a -> b) -> List<a> -> List<b> =
    \f. \ls.
        match ls {
            List::Nil() -> List::Nil(),
            List::Val(x, xs) -> List::Val(f x, map f xs),
        }
in
let fizzbuzz: Double -> String =
    \x.
        if eq (mod x 15) 0 then
            "FizzBuzz"
        else if eq (mod x 3) 0 then
            "Fizz"
        else if eq (mod x 3) 0 then
            "Buzz"
        else
            str x
in

map (\x. fizzbuzz x)  List::Val(1, List::Val(2, List::Val(3, List::Nil()))))
```

#### Args
```
val (type, result) = runProgram("""

    concat args_test (concat args_testBool (concat args_testInt "!!")))
    
""".trimIndent(), "test" to "KEK!", "testInt" to 4, "testBool" to false)
println("type: $type\nresult: $result")
```

#### When with comments! (WILL BE CHANGED)
```
# this is a when construction, an if the good way! #
(\a. when a as xs { # every when must rename its input expression using as, this means that you can eval things there! #
    eq xs 5 -> "55", # this is a basic branch: condition -> if condition is fullfilled call this #
    and (gt xs 10) (lt xs 100) -> "100",
    lt xs 200 -> "200",
    _ -> "1000",  # this is an else branch: _ -> if every other failed #
}) 231 # although else branch is not required, each thing in funlang MUST return something (not null!!) #
```

#### Fibonacci the good way
```
let rec fib =
    \n. \a. \b.
        when n as xs {
            eq xs 0 -> a,
            _ -> fib (sub n 1) (b) (add a b)
        }

in fib 200 0 1
```
