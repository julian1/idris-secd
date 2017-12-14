{-
    example shows 'normal' list consing... and matching
-} 

import Assembler

infixr 7 &


data Vm : Type where
  NIL : Vm                    -- call it start? or have a symbol opCode?
  (&) : OpCode -> Vm -> Vm    



human : Vm -> String
human NIL        = "nil"
human ((&) op xs ) = human xs ++ " " ++ human op


expr' : Vm
expr' = ADD & NIL


-- isns have to be reversed... because of operand order - which is really hard to read...
expr : Vm
expr = 
    ADD 
  & ADD
  & (PUSH1 $ Literal 0x01) 
  & NIL 


{-
mappend : Vm -> Vm -> Vm 
mappend c NIL = c
mappend NIL c = c
mappend x ((&) e' op') = x & e'
-}


-- if we're going to concat insns around then we will a concat

-- 
eval : Vm -> Integer
eval NIL = 0
eval ((&) ADD e) = eval e
eval ((&) SUB e ) = eval e


main : IO ()
main =
  printLn .human $ expr



-- (&) : Vm -> OpCode -> Vm
-- (&) = DOT
-- DOT : (Vm -> OpCode -> Nat ) ->  OpCode -> Vm -> Vm


