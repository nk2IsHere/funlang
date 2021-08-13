# i am a comment
let main =
  \args.
    (\a. when a as xs {
      eq xs 5 -> "55",
      and (gt xs 10) (lt xs 100) -> "100", #* Me too *#
      lt xs 200 -> "200",
      _ -> "1000", # me too!
    }) 231
