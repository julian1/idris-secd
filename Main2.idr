{-
  pushing stack usage into the expression...

  this is wrong - it should be the maximum stack use needed to evaluate the 
  expression.

  What’s happening here is that the Idris standard library defines plus on Nat
  in such a way that the Idris compiler can easily see that 1 + n is equal to S
  n, but cannot easily see this about n + 1:


  OK - we can actually do lambdas... with stack machine... which is very interesting.
  if we have a complicated lambda expression.... then the bound arguments...

  (\x . 1 + 3 + x) (x)

  the argument is in the wrong stack place - or is it...
  code would be, push 1, push 3, add, add

  (\x . x + 1 + 3) (x)

  push 1, add, push 3, add

  ----
  to call 
    - push current location, so we cant jump back.
    - push args.

-}

-- ok, I thnink we need a rewrite? 

{-
  - hang on - if we have the expression - then we can just calculate the stack usage...
  we don't have to push it into the types...
  and we're never really going to know - what the max is - anyway - when we have map/fold on unknown lists? 

  evaluation...
-}

data Expr : (depth : Nat) -> Type where

  Number : Integer -> Expr 1                     -- eg. pushing on stack is one gas.

  Add : Expr (S x) -> Expr (S y) -> Expr (x + y + 1)


  -- adding this - and we have the parse tree - not just a simple expression language
  Variable : String -> Expr 1           -- a variable in a lambda term.    

  Apply : Expr a -> Expr b -> Expr 1    -- eg. (\x -> x) 123

  Lambda : String -> Expr 1             -- we don't know the stack depth. until we're evaluating the thing... 
                                        -- we also need to have a symbol for replacement ... 

{-
  an expression term ought to be able to be represented by, 
    code pointer  - for lambda code...
    operand stack
-}

-- evaluate expression recursively
eval : Expr n -> Integer
eval x = case x of
  Number val => val
  Add lhs rhs => eval lhs + eval rhs



-- evaluating should leave a single value on the stack
eval' : Expr n -> { auto condition : n = 1 } -> Integer
eval' x = eval x


-- Generate evm code...
-- we need to control the recursion direction
gen : Expr n -> String
gen x = case x of
  Number val => 
    "push " ++ show val ++ "\n" -- ok - the values should be on the stack - need to check how this works...
  Add lhs rhs => 
    gen lhs 
    ++ gen rhs 
    ++ "add\n"



-- expr : { n : Nat } -> Expr n
expr :  Expr 1
expr =
  Add (Add (Number 102) (Number 103)) (Number 104)
  -- Add (Number 102) (Number 103) 

-- ok we want to expose the inner argument...

main : IO ()
main = do
  putStrLn $ "gas: " ++ show (eval' expr) -- TODO - expose the type value
  putStrLn $ "evaluated: " ++ show (eval expr)
  putStrLn (gen expr)

  

