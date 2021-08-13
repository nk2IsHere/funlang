let main =
  \args.
    concat (get args "test") (concat (str (get args "testBool")) (concat (get args "testInt") "!!"))
