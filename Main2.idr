
-- ok, don't knwo that we can even encode what we want...

-- perhaps we 

data Elem : Integer -> Type where

  Literal : Integer -> {gas : Integer} -> Elem gas
  Add : Elem x -> Elem x -> Elem ( x  )


elem1 : Elem gas 
elem1 = Literal 456


elem2 : Elem gas 
elem2 = Add (Literal 456) (Literal 3)


{-
elem2 : Elem 123
elem2 = Add (Literal 1) (Literal 123)
-}




f : Elem 4 -> Integer
f x = case x of
  Literal val => val
  Add lhs rhs => 123 



main : IO ()
main = print (f elem1)


