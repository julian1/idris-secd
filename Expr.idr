
module Expr

-- fully import so we don't have to prefix op-codes
import Assembler 


import Debug.Trace

-- TODO This shouldn't be mixed up here...

data Expr : Type where

  Number : Integer -> Expr        -- eg. pushing on stack is one gas.

  Add : Expr -> Expr  -> Expr
  Mul : Expr -> Expr  -> Expr
  Sub : Expr -> Expr  -> Expr

  If : Expr -> Expr  -> Expr -> Expr


  -- monadic - not a pure expression
  -- call(g, a, v, in, insize, out, outsize)
  Call : Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr  -> Expr

  -- create(v, p, s)       create new contract with code mem[p..(p+s)) and send v wei and return the new address
  Create : Expr -> Expr -> Expr -> Expr

  -- codecopy(t, f, s)   -   copy s bytes from code at position f to mem at position t
  CodeCopy : Expr -> Expr -> Expr -> Expr

  Gas : Expr
  Address : Expr
  Balance : Expr -> Expr
  MStore : Expr -> Expr -> Expr
  MLoad : Expr -> Expr
  Return : Expr -> Expr -> Expr
  CallDataSize : Expr

  Loader : Expr -> Expr -- it's a primitive

  Ops : List OpCode -> Expr

  -- side effects
  Seq : Expr -> Expr -> Expr

  -- log0(p, s)  -   log without topics and data mem[p..(p+s))
  Log0 : Expr -> Expr -> Expr


  -- lambda args - placeholders -- change to Integer for the placehodl
  Arg1 : Expr
  Arg2 : Expr

  -- we are going to need parenthesis, during parse...
  -- actually no... because we don't parse

  Apply : Expr -> Expr
    -- eg. (\x -> x) 123
    -- Apply2, Apply3 etc...



compile' : Expr -> List OpCode
compile' expr = [ POP ] 



compile : Expr -> List OpCode
compile expr = case expr of

  Number val =>
    if val <= 0xff then
      [ PUSH1 $ Literal val ]
    else if val <= pow 2 (2 * 8) then   -- ie 0xffff
      [ PUSH2 $ Literal val ]
    else if val <= pow 2 (20 * 8) then  -- ie 0xffffffffffffffffffffffffffffffffff
      [ PUSH20 $ Literal val ]
    else if val <= pow 2 (32 * 8) then
      [ PUSH32 $ Literal val ]
    else
      trace "ERROR: integer too large" []


  -- Change this to built-in BinOp or arith BinOp etc... though we might want to handle types
  -- not sure...
  Add a b => compile b ++ compile a ++ [ ADD ]
  Sub a b => compile b ++ compile a ++ [ SUB ]
  Mul a b => compile b ++ compile a ++ [ MUL ]

  -- relative jump labeling
  -- need more than one byte... and proof about len offset
  -- none of this list concat is efficient. probably should use join/flatten
  If pred a b =>
    let p = compile pred
        l = compile a
        r = compile b
        -- ll = toIntegerNat $ length l
        -- lr = toIntegerNat $ length r
    in
     p ++ [ ISZERO ]
      ++ [ PUSH1 $ Symbol "label1", JUMPI ]
      ++ l
      ++ [ PUSH1 $ Symbol "label2", JUMP ]
      ++ [ LABEL "label1", JUMPDEST  ]
      ++ r
      ++ [ LABEL "label2", JUMPDEST  ]

{-
      p ++ [ ISZERO ]
      ++ [ PUSH1 $ Left $ fromInteger (ll + 8), PC, ADD, JUMPI ]
      ++ l
      ++ [ PUSH1 $ Left $ fromInteger (lr + 4), PC, ADD, JUMP ]
      ++ [ JUMPDEST  ]
      ++ r
      ++ [ JUMPDEST  ]
-}
-- the ifelse code is not even using...

  -- stateful probably doesn't want to be modelled as expression syntax
  -- call(g, a, v, in, insize, out, outsize)
  -- call(g, a, v, in, insize, out, outsize), call contract at
  -- address a with input mem[in..(in+insize)) providing g gas and v wei and output area mem[out..(out+outsize)) ...
  Call g a v in_ insize out outsize =>
    let g' = compile g
        a' = compile a
        v' = compile v
        in_' = compile in_
        insize' = compile insize
        out' = compile out
        outsize' = compile outsize
    in
    outsize' ++ out' ++ insize' ++ in_' ++  v' ++ a'  ++ g' ++ [ CALL ]

  -- 
  -- create(v, p, s)       create new contract with code mem[p..(p+s)) and send v wei and return the new address
  Create v addr s => compile s ++ compile addr ++ compile v ++ [ CREATE ]

  -- codecopy(t, f, s)   -   copy s bytes from code at position f to mem at position t
  CodeCopy addr f s => compile s ++ compile f ++ compile addr ++ [ CODECOPY ]

  -- IMPORTANT - codecopy can be used in place of [ push, mstore ] for large literals, not just code
  -- val is confusing with v for value.

  -- raw ops
  Ops ops => ops

  Seq a b => compile a ++ compile b

  Gas => [ GAS ]
  Address => [ ADDRESS ]
  Balance addr => compile addr ++ [ BALANCE ]

  -- mstore(addr, v)
  MStore addr val => compile val ++ compile addr ++ [ MSTORE ]

  MLoad addr => compile addr ++ [ MLOAD ]

  -- return(p, s)  -   end execution, return data mem[p..(p+s))
  Return addr val => compile val ++ compile addr ++ [ RETURN ]

  CallDataSize => [ CALLDATASIZE ]

  -- log0(p, s)  -   log without topics and data mem[p..(p+s))
  Log0 addr val  => compile val ++ compile addr ++ [ LOG0 ]


  Loader expr =>
      let ops = compile expr in 
      let len = length' ops in
      simpleLoader len ++ ops


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


----------------
-- synonyms
-- these are just synonyms
-- is there a shorthand way of appropriating type?

-- interface with Number ...
Num Expr where
    (+) = Add
    (*) = Mul
    fromInteger n = Number n


add: Expr -> Expr -> Expr 
add = Add 

sub: Expr -> Expr -> Expr 
sub = Add 

ifelse: Expr -> Expr -> Expr -> Expr
ifelse = If

call : Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr  -> Expr
call = Call

create: Expr -> Expr -> Expr -> Expr 
create = Create

codecopy : Expr -> Expr -> Expr -> Expr 
codecopy = CodeCopy



ops : List OpCode -> Expr 
ops = Ops

(>>=) : Expr -> Expr -> Expr 
(>>=) = Seq

-- infixr 7 >> 



-- IMPORTANT - all the stateful operations - they should be using an append/ monoid. Not embedded in an expression.
-- particularly when use vars

mstore : Expr -> Expr -> Expr
mstore = MStore

return : Expr -> Expr -> Expr
return = Return

calldatasize : Expr
calldatasize = CallDataSize

log0 : Expr -> Expr -> Expr
log0 = Log0

minus : Expr -> Expr -> Expr
minus = Sub

mload : Expr -> Expr
mload = MLoad

gas : Expr
gas = Gas

address : Expr
address = Address

balance : Expr -> Expr
balance = Balance

loader : Expr -> Expr 
loader = Loader



-- http://docs.idris-lang.org/en/latest/tutorial/syntax.html
syntax "if" [test] "then" [t] "else" [e] = If test t e;

syntax "var" [name] = Variable name;

-- OK we can use {x} for bound variable names...
-- syntax define {fname} {arg} ":=" [exp] = Apply exp
-- syntax "lambda"  {arg} ":=" [exp] = Apply exp


----------------

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




main : IO ()
main = do

  -- let xxx = the (Either Integer String) $ Left 123
  -- the freaking offset calculation is a bit complicated...

  -- let ops = compile $ myfunc3 Arg1

  let ops = compile expr
  -- let ops = [ JUMPDEST , JUMPDEST, JUMPDEST , ISZERO ]
  printLn $ human' ops 


  printLn $ human' $ resolve ops 

  printLn $ machine' $ resolve ops
  -- printLn $ Strings.length $ machine' ops 

  -- printLn $ length' ops 


  -- printLn $ human' $ resolve $ simpleLoader 10


-- let ops = [ JUMPDEST , JUMPDEST, JUMPDEST , ISZERO ]
  printLn $ machine' $ resolve [ DATA8 $ Plus (Literal 3) (Literal 4) , JUMPDEST, JUMPDEST , ISZERO ]




  pure ()

  -- https://ropsten.etherscan.io/address/0xf5d27939d55b3dd006505c2fa37737b09ebacd71#code

  -- see the amount of data we were passed
{-
  let ops' =
      (compile calldatasize) ++ [ POP ]



  -- these are all stateful actions...

  -- push a value, log it, and return it
  let ops'' =
         (compile $ mstore 0x00 0xeeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffee )
      ++ (compile $ log0 0x00 32)
      ++ (compile $ return 0x00 32)

  -- works to call ourselves...
  let ops''' =
      [ PUSH1 $ Left 0xff, POP ]
       ++ (compile $ call gas address 0x0 0x0 0x0 0x0 0x0 ) -- call ourselves recursively...

  -- works - call contract with some data
  -- call(g, a, v, in, insize, out, outsize)
  let ops'''' =
      (compile calldatasize) ++ [ POP ] -- report how much data was passed..
      ++ (compile $ balance address ) ++ [ POP ] -- report how much data was passed..
      ++ (compile $ mstore 0x05 0xeeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffee ) -- store data at 05 ...
      ++ (compile $ call gas address 0x0 0x5 32 0x0 0x0 ) -- call passing the data

  -- works - sends 1 eth to address 0x, use hevm with --value flag.
  let ops''''' =
      (compile $ balance address ) ++ [ POP ]        -- show eth amount
      ++ (compile $ call gas 0x0 1 0x0 0x0 0x0 0x0 ) -- send 1 eth to 0x000000 
      ++ (compile $ balance address ) ++ [ POP ]     -- show eth amount
      ++ [ STOP ]



  let all =
      compile $
        codecopy 0 30 16                                    -- copy contract code to memory 0, code pos 30, len 16
        >>= create 0 0 16                                   -- create contract value 0, mem address 0, len 16
        >>= call gas (ops [ DUP 6 ]) 0  0x0 0x0 0x0 0x0     -- call contract, swapping in address
        >>= ops [ POP, POP, STOP ] 
        >>= (loader $ add 3 4)                                 -- simple contract to add two numbers - offset is 30 


  let hops = map human all
  printLn hops

  let mops = foldl (++) "" $ map machine all
  putStrLn mops

-}


-- myfunc4: Expr
-- myfunc4 = call 0x0 0x0 0x0 0x0 0x0 0x0 0x0


--mycall : Expr -> Expr

{-
With the exception of PUSH1, none of the opcodes have an argument
The argument of PUSH1 is also separated by white space
-}

-- ok, we want some loader code...
-- issue is that to call this. we're going to have to use a keccak i
-- actually we ought to be able to load it into whatever address that we want.
-- 0x100

{-
  VERY IMPORTANT.
  lets get codecopy as an intrinsic also.  same as call.

  actually - if we can't calculate the code offset - we might as well leave it as a literal opcodes...
    although would be a lot nicer when typed...

-}

