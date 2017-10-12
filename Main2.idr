{-
    simple gas expression...

-}

-- ok, don't knwo that we can even encode what we want...
-- perhaps we 

data Expr : Integer -> Type where

  -- Literal : Integer -> {gas : Integer} -> Expr gas
  Literal : Integer -> Expr 1             -- eg. pushing on stack is one gas. 
  Add : Expr x -> Expr y -> Expr ( x + y + 1 )


-- this is correct cost is 3 gas
elem2 : Expr 3
elem2 = Add (Literal 456) (Literal 3)


-- OK - we want to change this to be an evaluation function

f : Expr n -> Integer
f x = case x of
  Literal val => val
  Add lhs rhs => 123 

-- So I suspect the gas is not bound...
-- OK - this may not be able to be resolved because we don't know gas


main : IO ()
main = print (f elem2)


