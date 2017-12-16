
-- needs -p contrib, to pick up the Data.SortedMap.

-- change name Assem or Assembly?
module Assembler


-- import Debug.Error
-- import Debug.Trace
import Data.SortedMap

import Helpers

{-
 - good op-codes - eg. operands
  https://github.com/CoinCulture/evm-tools/blob/master/analysis/guide.md

  https://ethereum.stackexchange.com/questions/119/what-opcodes-are-available-for-the-ethereum-evm

 solidity primitives /assembly
  http://solidity.readthedocs.io/en/develop/assembly.html

 we need freaking tests to lockdown the behavior...
-}


infixr 7 &






{-
  simple expressions for symbols and literals
  VERY IMPORTANT width checking for operands should occur after resolution...
  and use the op-code context
  although this means data can be constructed that is not correct by construction ...
  could introduce another width checked operand???   
-}
public export
data AExpr : Type where
  Symbol  : String -> AExpr
  Literal : Integer -> AExpr
  Plus    : AExpr -> AExpr -> AExpr
  Sub     : AExpr -> AExpr -> AExpr



-- IMPORTANT - TODO if expr includes a label then decimal format. else hex format.
-- use show instead
-- 
humanE : AExpr -> String
humanE expr = case expr of
  Symbol sym  => sym
  Literal val => show val
  Plus l r    => humanE l ++  " + " ++ humanE r
  Sub l r     => humanE l ++  " - " ++ humanE r



-- OpCode or Inst or just Code
public export
data OpCode : Type where

  ADD     : OpCode
  MUL     : OpCode
  SUB     : OpCode

  ISZERO  : OpCode

  PUSH1   : AExpr -> OpCode
  PUSH2   : AExpr -> OpCode
  PUSH20  : AExpr -> OpCode
  PUSH32  : AExpr -> OpCode

  POP     : OpCode

  -- dup and swap could/should also use AExpr? but need to check widths...
  -- lets leave for moment. point is that they're separate opcodes with op-code numbers...
  -- and the symbols should reflect that
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
  
  -----------
  -- Non Opcodes. That can be embedded

  -- allow injecting raw data
  DATA8 : AExpr -> OpCode

  -- change name to label..., because there are other types of LABELs
  LABEL : String -> OpCode
  COMMENT : String -> OpCode



-- synonym for (::) refining the type makes it easier for the typechecker 
-- The confusing bit is nil is last... but it's the same as string concatenation
-- should move this opcode to Assembler...
public export
(&) :  OpCode -> List OpCode -> List OpCode
(&) =  Prelude.List.(::)



-- length of opcode representation
-- may want to use Nat here... if want to prove stuff...
export
length : OpCode -> Integer
length code =
  case code of
    LABEL _  => 0
    COMMENT _  => 0
    PUSH1  _  => 1 + 1
    PUSH2  _  => 1 + 2
    PUSH20 _  => 1 + 20
    PUSH32 _  => 1 + 32
    _         => 1


-- length of opcodes
export
length' : List OpCode -> Integer
length' xs =
  -- could probably be a lfold
  foldr f 0 xs where
    f code acc = acc + length code


-- human readable representation of code
-- show instead?
export
human : OpCode -> String
human expr = case expr of
  ADD     => "add"
  MUL     => "mul"
  SUB     => "sub"

  ISZERO  => "not"

  -- hmmm, we have to match all of these... each time...

  PUSH1  expr => "push1 " ++ humanE expr
  PUSH2  expr => "push2 " ++ humanE expr
  PUSH20 expr => "push20 " ++ humanE expr
  PUSH32 expr => "push32 " ++ humanE expr

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

  LABEL sym => "label " ++ sym
  -- DATA8 bits8 => "0x" ++ b8ToHexString bits8
  DATA8 expr => humanE expr

  COMMENT text => ""

export
-- think all this stuff is wrong...
human' : List OpCode -> String
human' ops = foldr f "" $ map human ops
  where f acc s = acc ++ ", " ++ s

{-
  THIS IS BETTER

human'' : List OpCode -> String
human'' ops = foldl f "" ops where
  f acc op = acc ++ ", " ++ human op

-}


-- machine representation of code
-- important - advantage of string instead of Bit8 is that we can ignore symbols...
-- alternatively we type the assembly as either opcode or symbol...
machine : OpCode -> String
machine expr = case expr of
  ADD     => "01"
  MUL     => "02"
  SUB     => "03"

  ISZERO  => "15"

  -- I think all of the formatting is a bit off...
  PUSH1  $ Literal val => "60" ++ showHex 1 val
  PUSH2  $ Literal val => "61" ++ showHex 2 val
  PUSH20 $ Literal val => "73" ++ showHex 20 val
  PUSH32 $ Literal val => "7f" ++ showHex 32 val

  -- all symbols should be resolved by this point
  -- but we would need a proof of that before we call...

  POP     => "50"

  -- need to constrain...
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

  -- ignore symbol
  LABEL  sym => ""

  -- DATA8 expr => b8ToHexString bits8
  DATA8 $ Literal val => showHex 1 val



-- do we need to fold right here??
export
machine' : List OpCode -> String
machine' ops = foldl (++) "" $ map machine ops



{-
  pass1, collect all the symbols, and their positions
  pass2, change the jump instructions
-}


-- TODO change name to Context?
SymsTy : Type
SymsTy = SortedMap String Integer

OpCodes : Type
OpCodes = List OpCode


-- evaluate an expression 
-- IMPORTANT - if expr includes a label then decimal format. else hex format.
-- use show instead
-- needs a context...
-- ahhh could pass down 

-- should avoid passing context down and just lexically bind. 

eval : SymsTy -> AExpr -> Integer
eval context expr = eval' expr
  where 
    eval' : AExpr -> Integer
    eval' expr = case expr of
      Symbol sym =>
        case lookup sym context of
          Just val => val
          Nothing => -9999    -- TODO fixme

      Literal val => val
      Plus l r    => eval' l + eval' r
      Sub l r    => eval' l - eval' r



-- resolve labels/symbols to literals
export
resolve : List OpCode -> List OpCode
resolve xs =
  let
    -- extract labels positions, and store in context
    (symbols,_) = foldl f (empty, 0) xs

    -- change ops to use positions
    xs' = foldr (f' symbols) Nil xs
  in xs'
  where
    -- store symbol and position
    f : (SymsTy, Integer) -> OpCode -> (SymsTy, Integer)
    f (m,len) (LABEL sym) = (insert sym len m, len)
    f (m,len) code         = (m, length code + len)

    -- replace expressions with literal value
    -- note, we cannot change the op-code with a narrower one, because
    -- it would change the computed lengths
    f' : SymsTy -> OpCode -> OpCodes -> OpCodes
    f' symbols code acc =
      case code of
        PUSH1 expr => PUSH1 $ Literal $ eval symbols expr
 
        DATA8 expr => DATA8 $ Literal $ eval symbols expr

        _ => code

      :: acc




-- this loader stuff belongs elsewhere

export
simpleLoader : Integer -> List OpCode
simpleLoader len =
  the (List OpCode) [
    PUSH1 $ Literal len,
    DUP 1,
    -- push1 0x0B,   -- we can use a symbol here... to handle this...
    PUSH1 $ Symbol "loader_finish",   -- we can use a symbol here... to handle this...
    PUSH1 $ Literal 0,
    CODECOPY,
    PUSH1 $ Literal 0,
    RETURN,
    LABEL "loader_finish"
  ];


export
addLoader : List OpCode -> List OpCode
addLoader ops =
  let len = length' ops in
  -- simple
  let loader = the (List OpCode) [
    PUSH1 $ Literal len, DUP 1, PUSH1 $ Literal 0x0B, PUSH1 $ Literal 0, CODECOPY, PUSH1 $ Literal 0, RETURN
    ];
  in
  -- like solidity
  let loader' = the (List OpCode) [
    PUSH1 $ Literal 0x60, PUSH1 $ Literal 0x40, MSTORE,
    PUSH1 $ Literal len, DUP 1, PUSH1 $ Literal 0x10, PUSH1 $ Literal 0, CODECOPY, PUSH1 $ Literal 0, RETURN
    ];
  in loader ++ ops


{-
  - should we type the width of the symbol. or have a string symbol?
   we want the ability to create symbols, not just encode labels.
   we can keep the symbol even after patching stuff in ...
   Needs a symbol ... rather than an integer. but when it's patched it will be an int...
   could use Either a b .... or else just use an integer and patch it up later...
   OK. a JUMP and JUMPI is going to need a constant that works like a push ...
   if doing a macro assemler - then a string would be easier -- I think a String might be easier...
   Actually rather than a raw string - might make a specific type for a symbol

   we also need to be able to insert a label for layout - without a jumpdest,
   actually we could split the jumpdest - to a symbol and a jumpdest
-}


-- lowercase op-codes are much nicer
-- But, it's not clear
{-
-- ease syntax
push1 : Integer -> OpCode
push1 x = PUSH1 $ Left x

push1' : String -> OpCode
push1' s = PUSH1 $ Right s
-}


{-
  Symbol representation...
    use a more sophisticated type? - to support local assembler expressions
      - eg. label1 + 10
   Integer -> Maybe String -> OpCode
   Maybe Integer -> Maybe String -> OpCode
   PUSH32 None Just "label1"   starts to get a bit complicated...
     the issue - is that we don't want the integer until we're ready
    (Either Integer String)
   or else have a different op-code...
   ok. hang on. keeping the symbol - after filling in the integer could be useful...
-}
{-
j : AExpr
j = Plus (Symbol "whoot") (Literal 0)

main : IO ()
main = do
  printLn $ humanE j

-}
{-
  PUSH1  $ Left val => "push1 0x" ++ showHex 1 val
  PUSH2  $ Left val => "push2 0x" ++ showHex 2 val
  PUSH20 $ Left val => "push20 0x" ++ showHex 20 val
  PUSH32 $ Left val => "push32 0x" ++ showHex 32 val

  PUSH1  $ Right sym => "push1 " ++ sym
  PUSH2  $ Right sym => "push2 " ++ sym
  PUSH20 $ Right sym => "push20 " ++ sym
  PUSH32 $ Right sym => "push32 " ++ sym
-}

