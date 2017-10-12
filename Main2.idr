{-
    simple gas expression...

-}

-- ok, don't knwo that we can even encode what we want...
-- perhaps we 

data Expr : Integer -> Type where

  -- Literal : Integer -> {gas : Integer} -> Expr gas
  Literal : Integer -> Expr 1             -- eg. pushing on stack is one gas. 
  Add : Expr x -> Expr y -> Expr ( x + y + 1 )



eval : Expr n -> Integer
eval x = case x of
  Literal val => val
  Add lhs rhs => eval lhs + eval rhs 


-- so we can know the amount of gas at compile time - and then limit what it does... 
-- we should also be able to evaluate it... 
-- we probably don't even need to embed the gas cost directly in the types...
-- we can embed it, in a evm compilation strategy... instead. which
-- eg. so that we can record stack use information 


-- this is correct cost is 3 gas
elem2 : Expr 3
elem2 = Add (Literal 102) (Literal 103)



main : IO ()
main = print (eval elem2)


