{-
  TODO - write the output to a file... then can load it in hevm in simple bash script.
  
-----
  IT WORKED - WE RETURNED A VALUE,
  the issue was we had the operands around the wrong way...

  the returned value is written to mem.
  then we return where in mem to find the value.

  echo 606060405260298060106000396000f37f0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF60005260206000f3 | ethrun 00 | json_pp | less

  NOTE - once we can call calldata - we can call constructor, then test, then tests all in the same thing.

  same thing ought to work ... in hevm as well...

  hevm exec --code 606060405260298060106000396000f37f0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF60005260206000f3 --calldata 00  --gas 1000

  also works when send transaction.

  HEVM - is designed to take code and calldata, and run both together.
    not deploy multiple contracts and then use it...

  But that means that - we can just remove the damn init/deploy code and test a contract...
    - but i think the limitation is testing calling of different contracts deployed at different addresses ...

  ----------------------------
    VERY IMPORTANT
    - we *can* run arbitrary code in a tx.  we do it by ommitting the 'to:' field
      and running the code we want. it will leave behind a residial contract which is fine.

  ----------------------------


-}

import Debug.Error
import Debug.Trace


%language ElabReflection
-- https://github.com/idris-lang/Idris-dev/tree/master/libs/contrib/Data
-- https://github.com/idris-lang/Idris-dev/blob/master/libs/prelude/Prelude/Interfaces.idr
-- https://github.com/idris-lang/Idris-dev/blob/master/libs/base/Debug/Trace.idr
-- https://wiki.dlang.org/Operator_precedence

-- good op-codes - eg. operands
-- https://github.com/CoinCulture/evm-tools/blob/master/analysis/guide.md

-- solidity primitives /assembly
-- http://solidity.readthedocs.io/en/develop/assembly.html

-- same as lambda but without comments...






data OpCode : Type where
  ADD     : OpCode
  MUL     : OpCode
  SUB     : OpCode

  ISZERO  : OpCode

  -- use integer for width ? to add operand...
  PUSH1   : Integer -> OpCode
  PUSH2   : Integer -> OpCode
  PUSH20  : Integer -> OpCode
  PUSH32  : Integer -> OpCode


  POP     : OpCode
  DUP     : Integer -> OpCode
  SWAP    : Integer -> OpCode

  MSTORE  : OpCode
  MLOAD   : OpCode

  JUMP    : OpCode
  JUMPI   : OpCode
  PC      : OpCode
  JUMPDEST : OpCode
  RETURN  : OpCode
  CALLDATASIZE : OpCode

  LOG0    : OpCode

  STOP    : OpCode    -- halts execution

  CALL     : OpCode
  CREATE   : OpCode
  CODECOPY : OpCode

  GAS      : OpCode
  ADDRESS  : OpCode
  BALANCE   : OpCode

  -- allow injecting raw data
  DATA : Bits8 -> OpCode




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




-- rename integerFromNat ?
natToInteger : Nat -> Integer
natToInteger = fromInteger . toIntegerNat


-- runtime assertion...
-- > :exec myassert "whoot" (>10) 12
myassert : String -> (a -> Bool) -> a -> a
myassert message p x =
  case p x of
    True => x
    False => trace message x



bytes : Integer -> List Integer
bytes x =
  let (_,xs) = f (x,[])
  in xs
  where
    f : (Integer, List Integer) -> (Integer, List Integer)
    f (0,acc) = (0, acc)
    f (x,acc) = f $ (div x 256, mod x 256 :: acc)



lpad : Nat -> a -> List a -> List a
lpad n x xs =
  replicate (n `minus` length xs) x  ++ xs




showHex : Nat -> Integer -> String
showHex w x =
  -- we should detect if the number overfills width ...
  let b = bytes x in
  let b' = lpad w 0 b in
  let b'' = myassert "ERROR: integer exceeds width" (\b => length b <= w) b' in
  foldr f "" $ b''
  where
    f x acc = (b8ToHexString .fromInteger) x ++ acc





-- a better way to get the length might be to format the opcodes
-- and then count them...
-- not sure. with an operand width this becomes easy
length' : List OpCode -> Integer
length' xs =
  -- needs to be left to right
  foldr (\expr, acc => acc + f expr) 0 xs
    where
    f expr = case expr of
      PUSH1  _ => 1 + 1
      PUSH2  _ => 1 + 2
      PUSH20 _ => 1 + 20
      PUSH32 _ => 1 + 32
      _ => 1


human : OpCode -> String
human expr = case expr of
  ADD     => "add"
  MUL     => "mul"
  SUB     => "sub"

  ISZERO  => "not"

  PUSH1  val => "push1 0x" ++ showHex 1 val
  PUSH2  val => "push2 0x" ++ showHex 2 val
  PUSH20 val => "push20 0x" ++ showHex 20 val
  PUSH32 val => "push32 0x" ++ showHex 32 val

  POP     => "pop"
  DUP val   => "dup" ++ show val -- integneeds to be tightened
  SWAP val  => "swap" ++ show val -- needs to be tightened

  MSTORE  => "mstore"
  MLOAD   => "mload"

  JUMP    => "jump"
  JUMPI   => "jumpi"
  PC      => "pc"
  JUMPDEST => "jumpdest"
  RETURN  => "return"
  CALLDATASIZE => "calldatasize"

  LOG0    => "log0"
  STOP    => "stop"

  CALL    => "call"
  CREATE  => "create"
  CODECOPY => "codecopy"

  GAS     => "gas"
  ADDRESS => "address"
  BALANCE => "balance"

  DATA bits8 => "0x" ++ b8ToHexString bits8


-- https://ethereum.stackexchange.com/questions/119/what-opcodes-are-available-for-the-ethereum-evm

--- TODO HUGHHH why is this a string rather than a hex value...
-- actually really doesn't matter too much

machine : OpCode -> String
machine expr = case expr of
  ADD     => "01"
  MUL     => "02"
  SUB     => "03"

  ISZERO  => "15"

  -- I think all of the formatting is a bit off...
  PUSH1  val => "60" ++ showHex 1 val
  PUSH2  val => "61" ++ showHex 2 val
  PUSH20 val => "73" ++ showHex 20 val
  PUSH32 val => "7f" ++ showHex 32 val

  POP       => "50"

  DUP val   => showHex 1 $ 0x7f + val
  SWAP val  => showHex 1 $ 0x8f + val

  MSTORE  => "52"
  MLOAD   => "51"

  JUMP    => "56"
  JUMPI   => "57"
  PC      => "58"
  JUMPDEST => "5b"
  RETURN  => "f3"
  CALLDATASIZE => "36"

  LOG0    => "a0"
  STOP    => "00"

  CALL    => "f1"
  CREATE  => "f0"
  CODECOPY => "39"

  GAS     => "5a"
  ADDRESS => "30"
  BALANCE => "31"

  -- TODO change name DATA to DATA?
  -- or DATA8 or BYTE8 ?
  DATA bits8 => b8ToHexString bits8


--
-- let v = fromInteger val in
-- the formatting must spit out the correct number of bytes...


simpleLoader : Integer -> List OpCode
simpleLoader len = 
  the (List OpCode) [
    PUSH1 len, DUP 1, PUSH1 0x0B, PUSH1 0, CODECOPY, PUSH1 0, RETURN
  ];



compile : Expr -> List OpCode
compile expr = case expr of

  Number val =>
    if val <= 0xff then
      [ PUSH1 val ]
    else if val <= pow 2 (2 * 8) then   -- ie 0xffff
      [ PUSH2 val ]
    else if val <= pow 2 (20 * 8) then  -- ie 0xffffffffffffffffffffffffffffffffff
      [ PUSH20 val ]
    else if val <= pow 2 (32 * 8) then
      [ PUSH32 val ]
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
        ll = toIntegerNat $ length l
        lr = toIntegerNat $ length r
    in
      p ++ [ ISZERO ]
      ++ [ PUSH1 $ fromInteger (ll + 8), PC, ADD, JUMPI ]
      ++ l
      ++ [ PUSH1 $ fromInteger (lr + 4), PC, ADD, JUMP ]
      ++ [ JUMPDEST ]
      ++ r
      ++ [ JUMPDEST ]


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



addLoader : List OpCode -> List OpCode
addLoader ops =
  let len = length' ops in
  -- simple
  let loader = the (List OpCode) [
    PUSH1 len, DUP 1, PUSH1 0x0B, PUSH1 0, CODECOPY, PUSH1 0, RETURN
    ];
  in
  -- like solidity
  let loader' = the (List OpCode) [
    PUSH1 0x60, PUSH1 0x40, MSTORE,
    PUSH1 len, DUP 1, PUSH1 0x10, PUSH1 0, CODECOPY, PUSH1 0, RETURN
    ];
  in loader ++ ops


-- Is this is part of a compilation...
-- or an actual function

-- its a function... with an argument...


{-
  I can use CODECOPY/RETURN to load a (non-solidity) contract into hevm with
  the single argument --code flag. To deploy multiple contracts - I am guessing
  the preferred way is with dynamic stub code that uses CREATE on each contract? 

  actually we can test this completely...  by doing it on a single contract...

  think the same process works. with 
-}



main : IO ()
main = do
  -- the freaking offset calculation is a bit complicated...

  -- let ops = compile $ myfunc3 Arg1
  -- let ops = compile $ myfunc0
  -- https://ropsten.etherscan.io/address/0xf5d27939d55b3dd006505c2fa37737b09ebacd71#code

  -- see the amount of data we were passed
  let ops' =
      (compile calldatasize) ++ [ POP ]

  -- push a value, log it, and return it
  let ops'' =
         (compile $ mstore 0x00 0xeeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffee )
      ++ (compile $ log0 0x00 32)
      ++ (compile $ return 0x00 32)

  -- works to call ourselves...
  let ops''' =
      [ PUSH1 0xff, POP ]
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



  -- AHHH perhaps create doesn't load the contract code from mem. instead loads loader code that loads the contract. 
  -- which will be created on return
  -- that might make things a lot easier.

  -- that is why we end up stepping into the create

  -- think we want to prove that the create works...
  -- if we use append and then a single compile. then we can insert position labels.
  -- and resolve it all ... after the fact... 

  -- OR.  we have PC so we can use relative...
  -- we have to - have both size and offset however - for memory operations...

  -- just a super-simple 
  -- we need to push and pop label values.... so I think that means it is higher than raw assembly... 
  
  -- having a kind of bind operator... 
  -- address and offset... is a property of the code....   It could just 
  -- we have a contract address..
  
  -- it could just be a final assembler pass?
  -- variable locations in mem. label locations in code.
  -- (label 'x')
  -- we could also introduce something in the assembly that just gets removed...
  -- eg. assembly is either an opcode. or a label.
  -- a label doesn't contribute to len ...

  -- TWO things - offset. and address..
  -- for an expression - we can calculate code, and know offset.
  -- don't think we can just embed a swap - actually we can.

  -- we have the contract creation address - as
  -- we leave the returned address from create on the stack. and then swap/dup it in. 
  -- or we use memory.
  -- actually we would just dup it in...

  -- which way does the stack count. if we can just do dup1 to get the top of stack then getting the address
  -- is easy...
  -- actually issue is we need to embed it in expression syntax... 
  -- top of stack is last pushed value
{-
    actually i think it's where we need the concept of a lambda ...
      what's happening behind the scenes is stack manipulation...
 
    a <- create ... 
    x <- call gas a v ...

    stack,mem,contect  -- all of this is mutated... throught >>=/ bind

    treat the VM as a tupple?  not for evaluation . where we have a
      - memory variables
      - stack variables , lambdas ?

    i think labels are a useful thing whatever we do...
    but to use them... we're going to have to have a single compile ...

    uggh we have label as a literal... 

      codecopy 0 label_123 16
      ...
      ops [ LABEL 123 ] 
    
    thing is that labels are not quit enough - need the size of code as well...

    for any piece of code. we need to know the size.

    VERY IMPORTANT...
      simpleLoader could take the compiled contract -- as an argument.
      eg. as an Expr
      
    Eg. it should take an expression as arg...
  --------
    
    all these operations are stateful - change tings. 
    a general purpose lambda... that can refer to the stack is going to be be useful...
    - the number of lambda args - is found  

    Applly is just push the arg. then jump to the expression.
    Q - what about return address?...

    Using a >>= \x -> b
    or just
      a >>= t 1 -> _1 
  
    even if not articulate properly is good.
    a single compile.
    stack variable versus mem variable.
-}
  {-
  let all_ =
          (compile $ codecopy 0 30 16 )                      -- copy contract code to memory 0, code pos 30, len 16
       ++ (compile $ create 0 0 16 )                         -- create contract value 0, mem address 0, len 16
       ++ (compile $ call gas (ops [ DUP 6 ]) 0  0x0 0x0 0x0 0x0 )   -- call contract, swapping in address
       ++ [ POP, POP, STOP ]                               -- padding
       -- ++ simpleLoader 5                                     -- contract loading 
       ++ (compile $ loader $ add 3 4)                                 -- simple contract to add two numbers - offset is 30 
  -}

  let all =
      compile $
        codecopy 0 30 16                                    -- copy contract code to memory 0, code pos 30, len 16
        >>= create 0 0 16                                   -- create contract value 0, mem address 0, len 16
        >>= call gas (ops [ DUP 6 ]) 0  0x0 0x0 0x0 0x0     -- call contract, swapping in address
        >>= ops [ POP, POP, STOP ] 
        >>= (loader $ add 3 4)                                 -- simple contract to add two numbers - offset is 30 



  -- make the loader a first class compile primitive.

  -- simpleLoader is a compilation step.
  -- swap 5 is a lambda argument consumption.

  -- question is - how to bind the expression. 
  -- just put the arguments in a list... 
  -- call compile on. if it's a lambda... we just
  -- lambda arg could be embedded.... in a nested the expression...
  -- so we have to keep a running understanding of the stack... 

  -- index 1,  index 2
  -- \arg => expr ( expr arg )   <- bound lambda expression. 
  -- expr ( expr _ 1)            <- bound lambda expression. 

  --------
  -- OK. i think what we do when we compile - is just pass down our stack depth in the expression.
  -- each argument will increase the stack depth...
  -- if we used a monad then we could increment it - all the way down...
    -- >>= compile x >>= compile y 
  -- don't think we can use swap though. we need to use dup. to allow using the same expression more than once. 
  -------

  -- let all = [ PUSH1 0x01, PUSH1 0x02, PUSH1 0x3, SWAP 2, STOP ] 

  let hops = map human all
  printLn hops

  let mops = foldl (++) "" $ map machine all
  putStrLn mops

  ------------------------
  -- WE NEED TO EXPRESSIONS like  (\addr -> call gas addr 0 0x0 0x0 0x0 0x0)
  ------------------------

  -- we could have a high level thing to embed op-codes...
  -- YES...

  -- when we do a create call, hevm changes context and gives us a screen of stop
  -- and returns value 0x01 which seems wrong.


  -- ok... we just want to wrap it up...

  -- note that we haven't actually pulled the data that we send off...
  -- passing arguments...

{-
  -- should be a separate function ...
  let all = the (List OpCode) $
     case False of
        True => addLoader ops
        False => ops
-}


{-
  -- QUESTION - can we arbitrarily manipulate the 0x40 0x60 stack pointer? when calling?
  -- eg. to change data, or short circuit?

  -- so lets try to push a value
TODO
  done - send bloody eth... to another address or contract.
    - can't see how to do this. we can send to ourselves. but that doesn't really show anything...
    - try sendTransaction to the contract we made - and then pass less eth a.
    - actually we could just burn it? by calling 0x00000 ? 

  see if can create a transaction, that is more than just the abi,understand what gets
    emitted from sendtransaction.


    VERY IMPORTANT
    - we *can* run arbitrary code in a tx.  we do it by ommitting the 'to:' field
      and running the code we want. it will leave behind a residial contract which is fine.

    - so we should try this.
    - eg. we call call against another contract. and can do whatever we want.
  ------------

  figure out how to call a contract with stack in a certain state etc..

  done - ok, so we can pass values in
  done - and return values out - and can log.

  done - figured out ethrun and hevm usage

  - implement a sequencing operation...
      even if not monadic...

  -- what we want to do now... have a code/function that calls another another contract...
  -- but not sure to do this cannot deploy more than one contract ...

  -- we really do want to use a call . To hit the constructor...
  -- actually next thing we want is access to the damn calldata.

  -- we can try calling our own
-}

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

{-

    address nameReg = 0x72ba7d8e73fe8eb666ea66babc8116a41bfb10e2;
    nameReg.call("register", "MyName");
    nameReg.call(bytes4(keccak256("fun(uint256)")), a);
        - can't interpret the return results...

  or web3,
    var contract = web3.eth.contract(contractABI).at(contractAddress);
    var callData = contract.functionName.getData(functionParameters);

  do solidity, callData might be reversed, https://ethereum.stackexchange.com/questions/27481/decoding-contract-output-of-web3-eth-call

  in and out are memory - not stack ...

    //callcode or delegatecall or call
      let retval := call(g
        , addr //address
        , 0 //value
        , o_code //mem in
        , calldatasize //mem_insz
        , o_code //reuse mem
        , 32) //Hardcoded to 32 b return value
    -----------------

  OK - VERY IMPORTANT.

  calldata is the calldata environment in the method. it's not a separate transaction to load...

  SO. am not sure it even works...
  ---

  so it doesn't use memory - for context boundaries..
  instead it uses calldata

  $ evm --debug --code 60003560203501 --input 00000000000000000000000000000000000000000000000000000000000000050000000000000000000000000000000000000000000000000000000000000004
VM STAT 6 OPs

  eg. add the values 4 and 5
  hevm exec --code 60003560203501  --calldata 00000000000000000000000000000000000000000000000000000000000000050000000000000000000000000000000000000000000000000000000000000004   --gas 1000 --debug

  CALLDATASIZE  - tells us the total data size. eg. 2x32 for the two args.
                - but we can play tricks.

  EG. we use div to right pad everywhere,
-}
