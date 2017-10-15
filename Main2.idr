{-
    pushing stack usage into the expression...

  this is wrong - it should be the maximum stack use needed to evaluate the 
  expression.

  What’s happening here is that the Idris standard library defines plus on Nat
  in such a way that the Idris compiler can easily see that 1 + n is equal to S
  n, but cannot easily see this about n + 1:
-}

-- ok, I thnink we need a rewrite? 

-- sub1 : Nat -> Nat
-- sub1 (S k) = k 


data Expr : (depth : Nat) -> Type where
  Literal : Integer -> Expr 1                     -- eg. pushing on stack is one gas.
  -- Add : Expr x -> Expr y -> Expr ( sub1 ( x + y))      -- adding should pop2 and add 1 - should be -1 - need to subtrac
  Add : Expr (S x) -> Expr (S y) -> Expr (x + y + 1)


-- evaluate expression recursively
eval : Expr n -> Integer
eval x = case x of
  Literal val => val
  Add lhs rhs => eval lhs + eval rhs


-- evaluating should leave a single value on the stack
eval' : Expr n -> { auto condition : LT n 6 } -> Integer
eval' x = eval x


-- Generate evm code...
-- we need to control the recursion direction
gen : Expr n -> String
gen x = case x of
  Literal val => "push " ++ show val ++ "\n" -- ok - the values should be on the stack - need to check how this works...
  Add lhs rhs => gen lhs ++ gen rhs ++ "add\n"


-- expr : { n : Nat } -> Expr n
expr :  Expr 1
expr =
  Add (Add (Literal 102) (Literal 103)) (Literal 104)
  -- Add (Literal 102) (Literal 103) 

-- ok we want to expose the inner argument...

main : IO ()
-- main = print (eval expr)
main = do
  putStrLn $ "gas: " ++ show (eval' expr) -- TODO - expose the type value
  -- putStrLn $ "evaluated: " ++ show (eval expr)
  putStrLn (gen expr)


