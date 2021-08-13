let flip =
    \f. \x. \y. f y x

let const =
    \x. \y. x

let main =
  \args.
    flip const "Henlo!" 2.4
