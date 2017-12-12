
import Assembler

infixl 7 &


-- 1. a structure - that can be destructured - and manipulated and sugar printed
-- 2. an evaluation function - necessary
-- 3. can do type level checks

-- this isn't a Vm it's a Vm and state transition.

data Vm : Type where
  NIL : Vm                    -- call it start? or have a symbol opCode?
  (&) : Vm -> OpCode -> Vm



human : Vm -> String
human NIL        = "whoot"
human ((&) a op) = "dot " ++ human a


expr : Vm
expr = 
  NIL 
  & (PUSH1 $ Literal 0x01) 
  & ADD 
  & ADD


eval : Vm -> Integer
eval NIL = 0
eval ((&) e ADD) = eval e


main : IO ()
main =
  printLn .human $ expr



-- (&) : Vm -> OpCode -> Vm
-- (&) = DOT
-- DOT : (Vm -> OpCode -> Nat ) ->  OpCode -> Vm -> Vm


