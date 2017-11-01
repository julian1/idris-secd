-- need to work out jump labels. can then do if/else, light calls, lambda, tail recursion, pairs etc.
-- problem with code-extraction is that the code is not proved.
-- problem with low level op-code - is that too far from workable programming - lambdas, free vars, lists. 
-- stack machinee makes hard.
-- working at level of expression tree - we don't have types...

-- Like LLL or solidity assembly. But with types.

-- OK we have

{-
-- what we need is some type of light call syntax. be it a lambda or something else.
-- push return code label, push arg, push arg.  jump to some code, leave return address.

  If we can do this. then we can write portable contracts and prove stuff.
    can implement lambdas on top of this if we really need it. etc.

  - so we can do this... as calls, not just expanded .
  (add (add (3) (4))

  push (jumpdest)
  push 123
  push 456
  jump (label or code)
  jumpdest

  if can do functions - then should be able to do if/else fairly easily as well.

  opcode return???
-}

{-
  an expression term ought to be able to be represented by,
    code pointer  - for lambda code...
    operand stack

    environment?
    resumption
    (secd)

  - substitute. if a value then just rewrite the expression...
      but this is beta/graph reduction. but what about not strict

  - non-strict - evaluate rhs/argument - and for each occurance dup the value on the stack
  - evaluate the argument, then for each bound variable dup the value on the stack.

  - we can't just dup anywhere in the code...  ahhhh - but we can if we have a pointer.
    or use a separate stack...

  - YES i think we can just use the stack for arguments. except we would dup before using...
    then pop args on function exit...
    (\x . 1 + 3 + x) (x)
-}

-- partial embedding
-- issues lambda lifting. want monads,
{-
data MyInteger =
  Z | 
  S Nat 

-- data MyInteger :  where

myadd : MyInteger -> MyInteger -> MyInteger 


||| Convert an Integer to a Nat, mapping negative numbers to 0
fromIntegerNat : Integer -> MyInteger 
fromIntegerNat 0 = Z 
fromIntegerNat n = 
  if (n > 0) then
    S (fromIntegerNat (assert_smaller n (n - 1)))
  else
    Z  

-}

-- hmmm, if we want a 

data Expr : Type where

  Number : Integer -> Expr        -- eg. pushing on stack is one gas.

  -- to remove ... this should be an environment
  Add : Expr  -> Expr  -> Expr

  If : Expr  -> Expr  -> Expr -> Expr


  -- lambda args - placeholders
  -- change to Integer for the placehodl
  -- should be able to remove
  Arg1 : Expr
  Arg2 : Expr

  Apply : Expr -> Expr        -- eg. (\x -> x) 123
  -- Apply2, Apply3 etc...



Num Expr where
    (+) = Add 
    (*) = Add 

    fromInteger n = Number n 





{-
  -- argument gets pushed on the stack.
    - but if/else will mean it may not be consumed...
  - instead at the time it gets used it needs to be duplicated... <- yes.
      but then we have to track the stack...
  - if we're doing let binding - then we're quite a way aways... from a simple embedding.

  lambda "x"
-}

-- variable resolution... do it on parse... 

-- actually we might be able to use a state like monad....
-- to keep track of the position?
-- But is embedding the fixed label position a good thing? when we otherwise have no such
--   restriction, on expanding expressions - and might do peephole stuff?

--- OK - we actually have to push the current position down into the expressions ...
----- actually no....    we can compute the length by the length of the string.... very interesting....
--- but it does have to be pushed down as an argument ...

---- IMPORTANT - we do have the PC . so we can do relative label. with just add and a jump

-- compile : Expr -> (pos: Integer) -> String
-- OK - we can get away with relative jump labels, which. and we can use the strlen to compute positions

-- OK rather than using hex - we should almost certainly just use a linked list of bytes.
-- then convert to hex later...
-- Actually, use a list of the Bits8 type

--- We should define the opcodes

-- opcode : Type

data OpCode : Type where
  ADD : OpCode
  ISZERO : OpCode
  PUSH : OpCode
  JUMP : OpCode
  JUMPI : OpCode
  PC : OpCode
  JUMPDEST : OpCode
  VAL : Bits8 -> OpCode


human : OpCode -> String
human expr = case expr of
  ADD => "add"
  ISZERO => "not"
  PUSH => "push"
  JUMP => "jump"
  JUMPI => "jumpi"
  PC => "pc"
  JUMPDEST => "jumpdest"
  VAL bits8 => "0x" ++ b8ToHexString bits8


-- https://ethereum.stackexchange.com/questions/119/what-opcodes-are-available-for-the-ethereum-evm

machine : OpCode -> String
machine expr = case expr of
  ADD => "01"
  ISZERO => "15"
  PUSH => "60"
  JUMP => "56"
  JUMPI => "57"
  PC => "58"
  JUMPDEST => "5b"
  VAL bits8 => b8ToHexString bits8



-- we need bound lambda vars. also free vars
-- perhaps free vars would be done better monadically?

-- do
-- Var x <- 123


compile : Expr -> List OpCode
compile expr = case expr of
  Number val =>
    let v = fromInteger val in
    [ PUSH, VAL v ]

  -- Change this to built-in BinOp or arith BinOp etc... though we might want to handle types 
  Add lhs rhs =>
    compile lhs
    ++ compile rhs
    ++ [ ADD ] --  are we doing this around the right way
  
  -- relative jump labeling
  -- none of this list concat is efficient. probably should use join/flatten 
  If cond lhs rhs =>
    let c = compile cond
        l = compile lhs
        r = compile rhs
        ll = toIntegerNat $ length l
        lr = toIntegerNat $ length r
    in
      c ++ [ ISZERO ]
      ++ [ PUSH, VAL $ fromInteger (ll + 8), PC, ADD, JUMPI ] 
      ++ l  
      ++ [ PUSH, VAL $ fromInteger (lr + 4), PC, ADD, JUMP ] 
      ++ [ JUMPDEST ] 
      ++ r  
      ++ [ JUMPDEST ] 


  -- var is on the stack so there's nothing to do...
  -- actually we want to dup it so we can refer to it again...
  -- BUT - how do we know to finish at the end of the function - easy just have a wrapper F
  Arg1 => [] 
  Arg2 => [] 

  -- look after a function
  -- can do something similar for arguments - eg. to evaluate them once.
  Apply e => 
      compile e
      -- and then pop off any arguments...



-- so we have a lambda expression. and we have an application... 
-- (\x -> x + 1) 1. 
-- the lambda expression can be alpha normalized... which is a transform...
-- but remember free lambda terms need resolution and lifting..

-- we don't even need to articulate the arguments - which are more like symbol lookup
-- just need the ordering.

expr :  Expr
expr =
  -- Add (Add (Number 10) (Number 1)) (Number 1)
  -- If (Number 0) (Add (Number 0x01) (Number 0x01))  (Add (Number 0x02) (Number 0x02))
  -- If (Number 1) (Add (Number 0x01) (Number 0x01))  ((Number 0x04) )
  -- If (Number 0) ((Number 0x01))  (Add (Number 0x02) (Number 0x02))

  If 0  (Add 0x01 0x01)  (Add (Number 0x02) (Number 0x02))


-- A lambda or function
-- to be valid in Idris has to take an input Expr and produce an output Expr
-- input is an arg

-- actually we might have higher level types - and then we evaluate... 
-- eg. a function that takes the lambda...

-- defining... a function...
add : Expr -> Expr -> Expr
add = Add

ifelse: Expr -> Expr -> Expr -> Expr
ifelse = If


-- This can be a placeholder for a function...
-- may want to specify function2 and function1 and function0, that way 
-- we know how to clean up the stack... 
-- also to compile - we have test args...

-- actually args might be better expressed explicitly...
function : Expr -> Expr 
function expr = Apply expr 

-----------------------------------------------------
--- OK here we have a weird function....
-- definining a function... that we might be able to make statements about...

-- add 1
myfunc: Expr -> Expr
myfunc arg = (Number 0x01) `add` arg 

-- add two functions
myfunc2: Expr -> Expr -> Expr
myfunc2 a b = function $ a `add` b 



-- It would be really nice if we could use an expression without a type constructor...

myfunc3: Expr -> Expr
myfunc3 c = 
  function $
    ifelse c 
      (1 `add` 1)  
      (Number 122)



-- so we have the generalized Expr... But what about the type of that expression...
-- Expr Integer 

-- actually
-- note that when we have a full tree - then we can do stuff... - like force eva


-- ok - we use bind?



--- ok this think actually works... i think...
-- this isn't very good...
-- OK - now how do we generate code for myfunc?


-- strategy for lambdas --- just expand the rhs - when we get to a variable binding 
-- we just dup the stack...
-- actually should probably remove the name? 
-- the only thing we need to kn

-- this is a high level parsing construct - we really only need the variable... 
-- Ahhh. actually it would be nice to deal with free variables

-- id:  Expr
--id = Lambda [ "x" ] (Variable "x")

-- an expr node has a type... eg. number...
-- but a lambda symbol doesn't....
-- symbol 

main : IO ()
main = do

  putStrLn "hi"

  -- let ops = compile expr
  -- let ops = compile $ myfunc2 Arg1 Arg2
  let ops = compile $ myfunc3 Arg1 
  let hops = map human ops
  let mops = foldl (++) "" $ map machine ops

  printLn hops
  printLn mops

-- myfunc arg = Apply (add (Number 0x01) (Variable "x") ) arg 
-- myfunc arg = Apply (add (Number 0x01) arg ) 

{-
  Variable : String -> Expr           -- a (bound) variable in a lambda term.

  -- Lambda : String -> Expr -> Expr     -- we don't know the stack depth. until we're evaluating the thing...
  Lambda : List String -> Expr -> Expr     -- we don't know the stack depth. until we're evaluating the thing...

  -- Apply : Expr -> Expr -> Expr        -- eg. (\x -> x) 123
                                       -- we also need to have a symbol for replacement ...

-}


{-
id:  Expr
id = Lambda "x" (Variable "x")

Apply id Number 123

      -- compile expr
      -- hops = map human ops


  -- putStrLn $ compile expr
  -- let yy = [ 1,2 ] ++  [4,5]

  -- let yy = (the 0x123 Bits8 ) : j in
  --let yy = 123 :: j in

-}

{-
*Lambda> :let x : Integer ; x = 123;
*Lambda> x
123 : Integer
*Lambda> the Bits8 x
-}
    -- let v = 123  in
    -- "whoot"
    -- let v = prim__truncInt_B8 val in
    -- b8ToHexString $ the Bits8 val
    -- b8ToHexString v
    --"push " ++ show val ++ "\n" -- ok - the values should be on the stack - need to check how this works...


{-
-- Generate evm code...
-- we need to control the recursion direction
compile : Expr -> String
compile x = case x of
  Number val =>
    "push " ++ show val ++ "\n" -- ok - the values should be on the stack - need to check how this works...
  Add lhs rhs =>
    compile lhs
    ++ compile rhs
    ++ "add\n"
-}
    {-
    compile lhs
    -- now compute the pos and pass that down...
    ++ compile rhs
    ++ "01"
    -}
{-
    -- push val
    let v = prim__truncBigInt_B8 val
        -- hex = b8ToHexString v
    in
    --"60" ++ hex --
    -- Not sure if we
-}
  -- ok, the issue is compileerating the labels...
  -- we're going to need to compute the instruction count.....
  -- use a different function ... or just return as a tuple?
  -- if we always push the lhs first... then we might be able to compileerate the label as we go...

    -- let v = prim__truncBigInt_B8 val in
