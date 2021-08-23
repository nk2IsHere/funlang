type List<a> { Val(a, List<a>), Nil() }

let map: forall a b. (a -> b) -> List<a> -> List<b> =
  \f. \ls.
    match ls {
      List::Nil() -> List::Nil(),
      List::Val(x, xs) -> List::Val(f x, map f xs),
    }

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

let main =
  \args.
    map (\x. fizzbuzz x)  List::Val(1, List::Val(2, List::Val(3, List::Nil())))
