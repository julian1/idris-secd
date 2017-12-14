{-
    Ok, the thing is if we switch to using this code,
    then a bunch of assembly stuff has to change as well...

    
-}
-- OK - it really should be possible to handle a secd environment this way also...
-- cleaner way to express - than using lists...
-- we can add typing constraints to the behavior of the VM 

import Assembler

infixl 7 &




data Vm : Type where
  NIL : Vm                    -- call it start? or have a symbol opCode?
  -- (&) : Vm -> OpCode -> Vm    -- should this be around the other way?
                              -- because that would be normal for a cons operator...
  (&) : OpCode -> Vm -> Vm    



human : Vm -> String
human NIL        = "nil"
human ((&) op xs ) = human xs ++ "dot " 


-- OK - this doesn't work is correct with, 'infixr 7 &'
expr' : Vm
expr' = (ADD & NIL)


{-
-- OK - this is correct with, 'infixr 7 &'
expr'' : Vm
expr'' = ADD & ADD & NIL
-} 

expr : Vm
expr = 
  NIL 
  & (PUSH1 $ Literal 0x01) 
  & ADD 
  & ADD


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
eval ((&) e ADD) = eval e
eval ((&) e SUB) = eval e


main : IO ()
main =
  printLn .human $ expr



-- (&) : Vm -> OpCode -> Vm
-- (&) = DOT
-- DOT : (Vm -> OpCode -> Nat ) ->  OpCode -> Vm -> Vm


