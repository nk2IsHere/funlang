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
