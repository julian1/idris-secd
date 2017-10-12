
-- ok, don't knwo that we can even encode what we want...
-- perhaps we 

data Elem : Integer -> Type where

  -- Literal : Integer -> {gas : Integer} -> Elem gas
  Literal : Integer -> Elem 1             -- eg. pushing on stack is one gas. 
  Add : Elem x -> Elem y -> Elem ( x + y + 1 )

--  Add : {a : Elem  x }  -> Elem y -> Elem ( x + y + 1 )
--   There : {x,y:a} -> {xs:Vect n a} -> IsElem x xs -> IsElem x (y :: xs)
  -- can we put a deriving...


elem1 : Elem 1 
elem1 = Literal 456


-- we have to express the gas cost...
elem2 : Elem 3
elem2 = Add (Literal 456) (Literal 3)



f : Elem n -> Integer
f x = case x of
  Literal val => val
  Add lhs rhs => 123 

-- So I suspect the gas is not bound...
-- OK - this may not be able to be resolved because we don't know gas


main : IO ()
main = print (f elem1)


