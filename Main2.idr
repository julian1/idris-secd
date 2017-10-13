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



-- we need to control the recursion direction 
gen : Expr n -> String
gen x = case x of
  Literal val => "push " ++ show val ++ "\n" -- ok - the values should be on the stack - need to check how this works... 
  Add lhs rhs => gen lhs ++ gen rhs ++ "add\n"



-- so we can know the amount of gas at compile time - and then limit what it does... 
-- we should also be able to evaluate it... 
-- we probably don't even need to embed the gas cost directly in the types...
-- we can embed it, in a evm compilation strategy... instead. which
-- eg. so that we can record stack use information 

-- we need a kecakk thing - so we can place code...


-- this is correct cost is 3 gas
expr : Expr 5
expr = 
  Add (Add (Literal 102) (Literal 103)) (Literal 104)
  -- (Add (Literal 102) (Literal 103)) 

-- ok we want to expose the inner argument...

main : IO ()
-- main = print (eval expr)
main = do 
  -- putStrLn $ "gas: " ++ show (eval expr) -- TODO - expose the type value
  putStrLn $ "evaluated: " ++ show (eval expr)
  putStrLn (gen expr)


