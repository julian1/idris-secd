{-
    pushing stack usage into the expression...

-}


data Expr : (depth : Nat) -> Type where
  Literal : Integer -> Expr 1                     -- eg. pushing on stack is one gas.
  Add : Expr x -> Expr y -> Expr ( x + y + 1 )    -- adding should pop2 and add 1 - should be -1
                                                  -- this should be a substraction... 

-- evaluate expression recursively
eval : Expr n -> Integer
eval x = case x of
  Literal val => val
  Add lhs rhs => eval lhs + eval rhs


-- if evaluating the expression would exceed the stack, it will fail to typecheck
eval' : Expr n -> { auto condition : LT n 1024 } -> Integer
-- eval' : Expr n -> { auto condition : LT n 6 } -> Integer
eval' x = eval x


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


