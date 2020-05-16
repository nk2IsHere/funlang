# funlang
Wannabe-Haskell language written on Kotlin

### How to run program:
> Simply import `funlang.interpret.runProgram` and call `runProgram`\
  Pair of inferred (if possible) Type and Result will be returned

### Standart functions:
`add 2 5` - adds two doubles\
`sub 5 2` - subtracts two doubles\
`mul 5 24` - multiplies two doubles\
`div 2 434` - divides two doubles\
`mod 10 3` - calculates modulus using two doubles\
`sqrt 10` - calculates square rectangle from double\
`gt(or gte, lt, lte) 3 3` - `>`, `>=`, `<`, `<=` using two doubles\
`concat "a" "!"` - adds two strings\
`str 2.13` - converts type to string

### TODO:
- I think I've forgotten to implement parsing of negative doubles, lol :D
- Write tests (😂👌👌👌👌)
- Supporting higher-order types
- Add examples on types and match constructions
- Add modules (separate files with types and its specific functions)
- Allow types to be declared everywhere (now those can be declared only on top of the program)
- Add more standart functions
- Fix type checker, sometimes it fails to correctly guess type when lambdas with Monotype.Var combine

### Examples:
#### Double (basically any number is double)
```
5.0
```

#### Bool
```
false
```

#### String
```
"I am string!"
```

#### If
```
if gt 2 3 then
    "2 > 3"
else 
    "You guessed it!"
```

#### Lambda
```
(\x. x) "Lambda can be called instantly like this!"
```

#### Let
```
let bangIt = 
    \x. 
        concat x "!" 
in 

bangIt "Hello and don't forget to try stacking multiple let's :D"
```

#### Higher-ordered Lambdas
```
let flip = 
    \f. \x. \y. f y x 
in

let const = 
    \x. \y. x 
in


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
in

fib 20
```

#### FizzBuzz with List type and Match!
```
type List<a> { Val(a, List<a>), Nil() }

let rec map : forall a b. (a -> b) -> List<a> -> List<b> =
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
