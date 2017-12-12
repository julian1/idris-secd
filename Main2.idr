
import Assembler

infixl 7 &

-- some stack representation
-- ok, this is now the composition function
-- this is a type or a
-- mapend...


-- this isn't a Vm it's a Vm and state transition.
data Vm : Type where
  NIL : Vm
  DOT : (Vm -> OpCode -> Nat ) -> Vm -> Vm



human : Vm -> String
human NIL     = "whoot"
human (DOT f a) = "dot " ++ human a



expr : Vm
expr = DOT (\vm, op => 123) $ NIL




-- expr = NIL

-- OK with a function then we can destructure it...
-- But we can't match 

-- NIL or identity
-- Dot : Vm -> OpCode -> Vm
-- Or it needs to be an actual function - with parenthesis?
-- Dot : (OpCode -> Vm)  -> Vm
-- maybe this needs to be the other way around -  Opcode ->  Vm ->

{-
-- what does this mean...
Dot (ADD vm) = vm
Dot (SUB vm) = vm
Dot (PUSH1 val) vm =  vm
-}


