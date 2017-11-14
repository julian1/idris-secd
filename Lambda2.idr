
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

  PUSH    : OpCode
  POP     : OpCode
  DUP1    : OpCode

  MSTORE  : OpCode 

  JUMP    : OpCode
  JUMPI   : OpCode
  PC      : OpCode
  JUMPDEST : OpCode
  RETURN  : OpCode
  STOP    : OpCode    -- halts execution

  CODECOPY : OpCode
  VAL : Bits8 -> OpCode


human : OpCode -> String
human expr = case expr of
  ADD     => "add"
  ISZERO  => "not"

  PUSH    => "push"
  POP     => "pop"
  DUP1    => "dup1" 

  MSTORE  => "mstore"

  JUMP    => "jump"
  JUMPI   => "jumpi"
  PC      => "pc"
  JUMPDEST => "jumpdest"
  RETURN  => "return"
  STOP   => "stop"

  CODECOPY => "codecopy"
  VAL bits8 => "0x" ++ b8ToHexString bits8


-- https://ethereum.stackexchange.com/questions/119/what-opcodes-are-available-for-the-ethereum-evm

--- TODO HUGHHH why is this a string rather than a hex value...
-- actually really doesn't matter too much

machine : OpCode -> String
machine expr = case expr of
  ADD     => "01"
  ISZERO  => "15"

  PUSH    => "60"
  POP     => "50"
  DUP1    => "80" 

  MSTORE  => "52"

  JUMP    => "56"
  JUMPI   => "57"
  PC      => "58"
  JUMPDEST => "5b"
  RETURN  => "f3"
  STOP    => "00"

  CODECOPY => "39"
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

{-
With the exception of PUSH, none of the opcodes have an argument
The argument of PUSH is also separated by white space
-}

-- ok, we want some loader code...
-- issue is that to call this. we're going to have to use a keccak i
-- actually we ought to be able to load it into whatever address that we want.
-- 0x100 

main : IO ()
main = do

  -- let ops = compile $ myfunc3 Arg1 
  let ops = compile $ myfunc0

  -- PUSH1 16 DUP PUSH1 12 PUSH1 0 CODECOPY PUSH1 0 RETURN STOP
  
  -- It may make sense to combine push and value pushed...
  -- because a push *has* to have a literal after it.... 
  -- And not sure any other value is like that...

  ------------------------
  -- OK 
  -- test 1 - try to get contract into call space. then call it at address after its deployed and get result
  -- test 2 - pass an argument and h

  let len = fromInteger .toIntegerNat .length $ ops 

  printLn len

  -- let loader = the (List OpCode) [ PUSH, VAL len, DUP1, PUSH, VAL 12, PUSH, VAL 0, CODECOPY, PUSH, VAL 0, RETURN, STOP ] ;
  -- let loader = the (List OpCode) [ PUSH, VAL len, DUP1, PUSH, VAL 12, PUSH, VAL 0, CODECOPY, POP, PUSH, VAL 0, RETURN, STOP ] ;
  let loader = the (List OpCode) [ PUSH, VAL len, DUP1, PUSH, VAL 11, PUSH, VAL 0, CODECOPY, PUSH, VAL 0, RETURN ] ;

  let loader = the (List OpCode) [ 
        PUSH, VAL 0x60, PUSH, VAL 0x40, MSTORE, 
        PUSH, VAL len, DUP1, PUSH, VAL 0x10, PUSH, VAL 0, CODECOPY, PUSH, VAL 0, RETURN 
        ] ;


  -- lets keep the 12. then change to 11.
  -- then try the full other example... with variables...

  -- solidity output example. our stuff looks correct.
    -- PUSH1 0x60 PUSH1 0x40 MSTORE 
    -- PUSH1 0x6 DUP1 PUSH1 0x10 PUSH1 0x0 CODECOPY PUSH1 0x0 RETURN 
    -- PUSH1 0x60 PUSH1 0x40 MSTORE STOP


{-
  When a contract creating transaction makes its way into the blockchain, the
  data bytearray in the transaction is interpreted as EVM code, and the value
  returned by that EVM execution is taken to be the code of the new contract

  RETURN, returning memory bytes 0-16,   eg. returns a range. 
-}

  -- so I think the idea is we return the address ie. 0. and the size. ie straight from codecopy
  -- but it doesn't work.

  let all = loader ++ ops


  let hops = map human all
  printLn hops

  let mops = foldl (++) "" $ map machine all
  printLn mops



