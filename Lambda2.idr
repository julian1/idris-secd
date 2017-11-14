
-- same as lambda but without comments...


data Expr : Type where

  Number : Integer -> Expr        -- eg. pushing on stack is one gas.

  Add : Expr -> Expr  -> Expr

  If : Expr -> Expr  -> Expr -> Expr

  -- lambda args - placeholders -- change to Integer for the placehodl
  Arg1 : Expr
  Arg2 : Expr

  Apply : Expr -> Expr        
    -- eg. (\x -> x) 123
    -- Apply2, Apply3 etc...


-- interface with Number ...
Num Expr where
    (+) = Add 
    (*) = Add 
    fromInteger n = Number n 


data OpCode : Type where
  ADD     : OpCode
  ISZERO  : OpCode
  DUP1    : OpCode
  PUSH    : OpCode
  JUMP    : OpCode
  JUMPI   : OpCode
  PC      : OpCode
  JUMPDEST : OpCode
  CODECOPY : OpCode
  RETURN  : OpCode
  STOP    : OpCode    -- halts execution
  VAL : Bits8 -> OpCode


human : OpCode -> String
human expr = case expr of
  ADD     => "add"
  ISZERO  => "not"
  DUP1    => "dup1" 
  PUSH    => "push"
  JUMP    => "jump"
  JUMPI   => "jumpi"
  PC      => "pc"
  JUMPDEST => "jumpdest"
  CODECOPY => "codecopy"
  RETURN  => "return"
  STOP   => "stop"
  VAL bits8 => "0x" ++ b8ToHexString bits8


-- https://ethereum.stackexchange.com/questions/119/what-opcodes-are-available-for-the-ethereum-evm

--- TODO HUGHHH why is tris a string rather than a hex value...

machine : OpCode -> String
machine expr = case expr of
  ADD     => "01"
  ISZERO  => "15"

  DUP1    => "80" 
  PUSH    => "60"
  JUMP    => "56"
  JUMPI   => "57"
  PC      => "58"
  JUMPDEST => "5b"
  CODECOPY => "39"
  RETURN  => "f3"
  STOP    => "00"
  VAL bits8 => b8ToHexString bits8



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

ifelse: Expr -> Expr -> Expr -> Expr
ifelse = If



expr : Expr
expr =
  -- Add (Add (Number 10) (Number 1)) (Number 1)
  -- If (Number 0) (Add (Number 0x01) (Number 0x01))  (Add (Number 0x02) (Number 0x02))
  -- If (Number 1) (Add (Number 0x01) (Number 0x01))  ((Number 0x04) )
  -- If (Number 0) ((Number 0x01))  (Add (Number 0x02) (Number 0x02))
  -- If 0  (Add 0x01 0x01)  (Add (Number 0x02) (Number 0x02))
  ifelse 0 (1 + 5) (2 + 2)


-- Think we need to be explicit with the args....
-- we ought to be able to simplify stuff.
function : String -> Expr -> Expr 
function s expr = Apply expr



-- OK there is support with dsl support... - including with lambdas.

-- http://docs.idris-lang.org/en/latest/tutorial/syntax.html
syntax "if" [test] "then" [t] "else" [e] = If test t e;

-- no different to just a function called lambda ... 
-- but we might be able to use it more effectively...
-- syntax "lambda" [arg] [exp] = Apply exp;

syntax "var" [name] = Variable name;

-- OK we can use {x} for bound variable names...
-- syntax define {fname} {arg} ":=" [exp] = Apply exp 
-- syntax "lambda"  {arg} ":=" [exp] = Apply exp 



myfunc0: Expr
myfunc0 = 0xaa + 0xbb 

-- add 1
myfunc1: Expr -> Expr
-- myfunc arg = (Number 0x01) `add` arg 
myfunc1 arg = 1 + arg 

-- add two functions
myfunc2: Expr -> Expr -> Expr
myfunc2 a b = function "myfunc2" $ a + b 


myfunc3: Expr -> Expr
myfunc3 c = 
  function "myfunc3" $
    if c 
      then (1 + 456) 
      else 123 


ops2 : List OpCode
ops2 = [ PUSH, VAL 16, DUP1, PUSH, VAL 12, PUSH, VAL 0, CODECOPY, PUSH, VAL 0, RETURN, STOP ];


-- ok, we want some loader code...
-- issue is that to call this. we're going to have to use a keccak i
-- actually we ought to be able to load it into whatever address that we want.
-- 0x100 

main : IO ()
main = do

  -- let ops = compile $ myfunc3 Arg1 
  let ops = compile $ myfunc0
  let hops = map human ops
  let mops = foldl (++) "" $ map machine ops

  printLn hops
  printLn mops

  -- PUSH1 16 DUP PUSH1 12 PUSH1 0 CODECOPY PUSH1 0 RETURN STOP
  -- lets ops = [ PUSH1 16, DUP, PUSH1 12, PUSH1 0, CODECOPY, PUSH1 0, RETURN, STOP ] 
--  ] ;--, 

  -- let ops2 = [ PUSH ]  ;
    -- where ops2 : List OpCode; 

  let ops3 = the (List OpCode) ([ PUSH, VAL 16, DUP1, PUSH, VAL 12, PUSH, VAL 0, CODECOPY, PUSH, VAL 0, RETURN, STOP ]) ;

{-
  let
    ops3 : List OpCode
    ops3 = [ PUSH, VAL 16, DUP1, PUSH, VAL 12, PUSH, VAL 0, CODECOPY, PUSH, VAL 0, RETURN, STOP 
-}


  printLn $ length hops 

