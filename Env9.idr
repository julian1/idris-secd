{-
  use separate cons/destrucuring operators

  ++ is right associative...
-}

data Item : Type where
  Nil : Item
  (::)  : Integer -> Item -> Item
  (++)  : Item -> Item -> Item          -- an embedded list

-- everything on s will be destured with ++

Show Item where
  show Nil           = ""
  show (val :: xs)   = show val ++ ", " ++ show xs
  show (val ++ xs)   = "(" ++ show val ++ "), " ++ show xs 



main : IO ()
main = do

  -- let e = the Item $ L [  L [ C 1, C 3 ] , L [ C 4 , L [ C 5 , C 6 ] ]  ]  

  -- e = ((1 3) (4 (5 6))).
  -- let e = the Item $ [ 1, 2, 3] ++ [ 4,5,6 ] 
  let f = the Item $ [ 5,6 ] 
  let e = the Item $ [ 1, 3 ] ++ [ 4 ]  ++ [ 7, 8 ] 
  printLn e

  let (m1 ++ m2) = e 
  printLn m1 

  putStrLn "-----"

