
{-
  This is just the same as 1 :: 2 :: NIL  and is fine.
  let ret = NIL Dot ADD Dot ADD

  But why can't we match on (Dot). Perhaps there's something else needed...
-}
-- example using the opcodes and composition functions
-- op codes and composition

-- probably just need (Dot)

import Assembler

infixl 7 &

-- some stack representation
-- ok, this is now the composition function
-- this is a type or a
-- mapend...


-- this isn't a Vm it's a Vm and state transition.
data Vm : Type where
  -- NIL or identity
  NIL : Vm
  -- Dot : Vm -> OpCode -> Vm
  Dot : OpCode -> Vm -> Vm

-- Or it needs to be an actual function - with parenthesis?
-- Dot : (OpCode -> Vm)  -> Vm
-- maybe this needs to be the other way around -  Opcode ->  Vm ->


-- what does this mean...
Dot ADD vm = vm
Dot SUB vm = vm
Dot (PUSH1 val) vm =  vm


human : Vm -> String
human NIL = "whoot"
-- human vm  = case vm of
--              ((Dot) _ _ ) => "whoot"

-- We can't match on Vm - because it's not matchable ...
-- human (Dot code NIL) = "whoot"


(&) : OpCode -> Vm -> Vm
(&) = Dot


-- we should be able to compose something together that can be matched...
-- and


main : IO ()
main = do

  -- the thing is - that this entire expression is evaluated...
  -- is that what we want?
  -- let ret = (\x => x) Dot ADD Dot ADD

  -- let ret = ADD & ADD & NIL  

  let ret = Dot ADD $ Dot ADD NIL  

  printLn "hi"



  -- not sure why this doesn't work...
  -- let ret = (\a => f a ADD ) . (f NIL ADD)
  -- precedence is around the wrong way???
  -- Ok this composes in the right direction....


{-
old
-- our transition function
-- need to change around - so that it's   (op a b)  not (a op  b) ?
f : Vm -> OpCode -> Vm
f vm ADD = vm
f vm SUB = vm
f vm (PUSH1 val) = vm


ret : Vm -> Vm
-}


