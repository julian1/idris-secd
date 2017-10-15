{-
    simple gas expression...

-}

-- ok, don't knwo that we can even encode what we want...
-- perhaps we 

data Expr : Nat -> Type where
  Literal : Integer -> Expr 1                     -- eg. pushing on stack is one gas. 
  Add : Expr x -> Expr y -> Expr ( x + y + 1 )

-- 

-- OK, hang on we're doing something funny ... we can't ensure that the condition always holds... 
-- it's recursive so on the lea
-- it is only the top level evaluation that should hold. 

-- evaluate expression recursively
eval : Expr n -> Integer
eval x = case x of
  Literal val => val
  Add lhs rhs => eval lhs + eval rhs 


eval' : Expr n -> { auto condition : GT n 0 } -> Integer
eval' x = eval x


-- 
-- Generate evm code...
-- we need to control the recursion direction 
gen : Expr n -> String
gen x = case x of
  Literal val => "push " ++ show val ++ "\n" -- ok - the values should be on the stack - need to check how this works... 
  Add lhs rhs => gen lhs ++ gen rhs ++ "add\n"


expr : Expr 5
expr = 
  Add (Add (Literal 102) (Literal 103)) (Literal 104)

-- ok we want to expose the inner argument...

main : IO ()
-- main = print (eval expr)
main = do 
  putStrLn $ "gas: " ++ show (eval' expr) -- TODO - expose the type value
  -- putStrLn $ "evaluated: " ++ show (eval expr)
  putStrLn (gen expr)


