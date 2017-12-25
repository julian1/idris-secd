

import Assembler 
-- import Expr



data Expr : Type where

  Number : Integer -> Expr

  -- a reference to a var might be free or bound
  -- if free - might be let bound, or might be lambda arg of closed scope
  Var : String -> Expr

  -- Abstraction - single argument expression - we need to name it to identify it...
  -- can rewrite to integers later.
  Abstraction : String  -> Expr -> Expr

  Apply : Expr -> Expr -> Expr

  -- let y = m in n
  Let : String -> Expr -> Expr -> Expr

  -- Add
  Binop : Expr -> Expr  -> Expr

  Builtin : List Expr  -> Expr
  -- How do we handle built in functions - eg. code copy....



main : IO ()
main = do

  let i = Apply (Abstraction "x" (Var "x")) (Number 123)

  let j =
    Let "y" (Apply (Abstraction "x" (Number 123)) (Var "x"))  $
    Let "y1" (Apply (Abstraction "x" (Number 123)) (Var "x")) $
    (Number 123)

  let k =
    Let "y" (Number 123) $
    Let "y1" (Number 456) $
    Binop (Var "y") (Var "y1")

  let l =
    Builtin [ Var "x", (Number 123) ]

{-
  let code = machine' . resolve . compile . L $
      balance address
      ^ asm [ POP ]
      ^ call gas 0x0 1 0x0 0x0 0x0 0x0

    PUSH1 $ Literal len, DUP 1, PUSH1 $ Literal 0x0B, PUSH1 $ Literal 0, CODECOPY, PUSH1 $ Literal 0, RETURN
-}

  -- ok compile, abstraction we have to push the stuff 

  -- only at apply should we push the argument... - and what about the env.
  let ops = the (List OpCode) [

    -- push return address...
    PUSH1 (Literal 5),
    PC,                           -- push the current PC for the return address, must use this as stack...
    ADD,

    -- push jump symbol and jump ..
    PUSH1 (Symbol "mylambda"), 
    JUMP,
  
    -- return jumpdest
    JUMPDEST , 
    STOP,
    STOP,

    -- my lambda function
    LABEL "mylambda", 
    JUMPDEST , 

    -- jump back using value on stack
    JUMP

  ]

  writeFile "out.vm" ops

  putStrLn "whoot"




-- abstraction
--    push the body (jump point), env (address), and var on the stack - as three separate items? p120
--
--    then if we were using a let binding to assign - we would pop these three things to env as memory.
--    OK.
--     let j = (\x.x) in j 123
--    but - importantly we must know that the var has type of lambda - and is not an integer. for instance.
--    eg. if the type of the var is lambda, then reading it would be popping the three vars of the lambda on the stack.
--    ok. I think this works.

-- assignment of abstraction to var
--    would push stack to env variable
--
-- read of var that's a lambda would pop those three vars
-- apply.
--   easy. just push the return address and do the jump.

-- a lambda is also going to be responsible for knowing whether the arg on the stack is simple, or a env / memory  reference.

-- abstraction - should just be a jumpdest and some code, andi
    -- C need to push the return address, and jump pop to return.
    -- E but also need the environment - can we push an address for environment to restore.

    -- the env has to be captured at the time of abstraction. abstraction is a run time operation.
    -- should just be a register pointing to mem?

-- apply
    -- push arg. push ret address. push current env?
-- application

-- OK - there's a problem. A builtin like codecopy can still be partially applied.
-- if we use the (Apply a b) - then the actual op has to be represented in memory somehow...
-- and a closure, closes over the environment in memory. Not the damn stack.

-- if we were going to create a lambda of a call - then we'd need to put everything into
-- mem/env. then if we apply we'd push everything on the stack and call...

-- OKK
{-
-- eg. (\x -> x) 123
-- Apply2, Apply3 etc...

-- lambda args - placeholders
-- change to Integer for the placehodl
-- should be able to remove
Arg1 : Expr
If : Expr -> Expr  -> Expr -> Expr
-}

