
-- same as lambda but without comments...


data Expr : Type where

  Number : Integer -> Expr        -- eg. pushing on stack is one gas.

  Add : Expr -> Expr  -> Expr

  If : Expr -> Expr  -> Expr -> Expr


  --call(g, a, v, in, insize, out, outsize)    
  -- not a pure expression
  Call : Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr  -> Expr 

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
  CALL     : OpCode

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
  STOP    => "stop"

  CODECOPY => "codecopy"
  CALL    => "call"

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
  CALL    => "f1"

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


  -- stateful probably doesn't want to be modelled as expression syntax
  -- call(g, a, v, in, insize, out, outsize)    
  Call g a v in_ insize out outsize => 
    let g' = compile g
        a' = compile a
        v' = compile v
        in_' = compile in_
        insize' = compile insize
        out' = compile out
        outsize' = compile outsize
    in
    g' ++ a' ++ v' ++ in_' ++ insize' ++ out' ++ outsize' ++ [ CALL ]


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


call : Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr  -> Expr 
call = Call

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


myfunc4: Expr
myfunc4 = call 0x0 0x0 0x0 0x0 0x0 0x0 0x0 


--mycall : Expr -> Expr

{-
With the exception of PUSH, none of the opcodes have an argument
The argument of PUSH is also separated by white space
-}

-- ok, we want some loader code...
-- issue is that to call this. we're going to have to use a keccak i
-- actually we ought to be able to load it into whatever address that we want.
-- 0x100 

main' : IO ()
main' = do

  -- let ops = compile $ myfunc3 Arg1 
  let ops = compile $ myfunc0

  let len = fromInteger .toIntegerNat .length $ ops 

  printLn len

  -- this works without var setup.
  let loader' = the (List OpCode) [ 
        PUSH, VAL len, DUP1, PUSH, VAL 0x0B, PUSH, VAL 0, CODECOPY, PUSH, VAL 0, RETURN 
        ];

  -- hmmm return looks like it returns immediately
  -- OR maybe --------   we just do lots of codecopy but don't return

  -- this works - as solidity like setup.
  let loader = the (List OpCode) [ 
        PUSH, VAL 0x60, PUSH, VAL 0x40, MSTORE, 
        PUSH, VAL len, DUP1, PUSH, VAL (0x10 + 0), PUSH, VAL 0, CODECOPY, PUSH, VAL 0, RETURN  --,
        -- PUSH, VAL 0x00, PUSH, VAL 0xff
        ];

  let all = loader ++ ops

  let hops = map human all
  printLn hops

  let mops = foldl (++) "" $ map machine all
  printLn mops



-- Ahhhh will might struggle to push a large integer? 

main : IO ()
main = do

  -- address value is truncated....

  let ops = compile $ call 0xffff 0x0 0x0 0x0 0x0 0x0 0x0 
  let len = fromInteger .toIntegerNat .length $ ops 

  printLn len
  
  let all = ops -- loader ++ ops

  let hops = map human all
  printLn hops

  let mops = foldl (++) "" $ map machine all
  printLn mops






 ------------------------
  -- OK 
  -- test 1 - try to get contract into call space. 
  -- test 2 - try to call it at address after its deployed and get result
  -- test 2 - pass an argument and h
  -- lets keep the 12. then change to 11.
  -- then try to call the contract - 
  -- then try the full other example... with variables...

  -- IMPORTANT - we should try to do this in hevm. deploy the contract, then jump into it.

  -- we have code to deploy. but we have to return before we can execute it...



{--
call(g, a, v, in, insize, out, outsize)    
  call contract at address a with input mem[in..(in+insize)) providing g gas 
  and v wei and output area mem[out..(out+outsize)) returning 0 on error (eg. out 
  of gas) and 1 on success

  - (in,insize) (out,outsize)

-}



  -- so we can init/loader code in at an address using codecopy ... 
  -- but how do we 
  -- ahhh --- if it doesn't indicate stop maybe we get called back again?

{-
  we just need a raw transaction...
  
  web3.eth.sendTransaction({
  from: sendingAccount,
  to: contract.address,
  data: yourData, // optional, if you want to pass data or specify another function to be called by delegateCall you do that here
  gas: requiredGas, // technically optional, but you almost certainly need more than the default 21k gas
  value: value //optional, if you want to pay the contract Ether


  a simple assembler would be really nice as well... with () instead of application.

<address>.balance (uint256):
    balance of the Address in Wei
<address>.transfer(uint256 amount):
    send given amount of Wei to Address, throws on failure
<address>.send(uint256 amount) returns (bool):
    send given amount of Wei to Address, returns false on failure
<address>.call(...) returns (bool):
    issue low-level CALL, returns false on failure
<address>.callcode(...) returns (bool):
    issue low-level CALLCODE, returns false on failure
<address>.delegatecall(...) returns (bool):
    issue low-level DELEGATECALL, returns false on failure 

});


-}

  -- OK we should be able to all it - just calling call(), callcode() or delegatecall() from ordinary code...
  -- can we do this from web3 however...
  -- HOW... did the guy call...

{-
  When a contract creating transaction makes its way into the blockchain, the
  data bytearray in the transaction is interpreted as EVM code, and the value
  returned by that EVM execution is taken to be the code of the new contract

  RETURN, returning memory bytes 0-16,   eg. returns a range. 
-}

