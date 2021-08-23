let memoize fib =
    \x.
        if or (eq x 1) (eq x 2) then
            1
        else
            add (fib (sub x 1)) (fib (sub x 2))

let main =
  \args.
    fib 20
