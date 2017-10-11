
-- ok, don't knwo that we can even encode what we want...
-- perhaps we 

data Elem : Integer -> Type where

  -- Literal : Integer -> {gas : Integer} -> Elem gas
  Literal : Integer -> Elem 1             -- eg. pushing on stack is one gas. 
  Add : Elem x -> Elem y -> Elem ( x + y + 1 )


elem1 : Elem 1 
elem1 = Literal 456


elem2 : Elem 3
elem2 = Add (Literal 456) (Literal 3)


-- So I suspect the gas is not bound...

{-
f : Elem 4 -> Integer
f x = case x of
  Literal val => val
  Add lhs rhs => 123 



main : IO ()
main = print (f elem1)

-}
