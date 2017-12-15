{-
    example shows 'normal' list consing... and matching
    lists have a lot of advantages... we get folds mappend etc without having to .

    OK - hang on a monadic bind puts the function second first.
-} 

import Assembler

infixr 7 &


data Vm : Type where
  NIL : Vm                    -- call it start? or have a symbol opCode?
  (&) : OpCode -> Vm -> Vm    



human : Vm -> String
human NIL          = "nil"
human ((&) op xs ) = human xs ++ " " ++ human op


expr' : Vm
expr' = ADD & NIL


-- isns have to be reversed... because of operand order - which is really hard to read...
-- although we could use a list and then build it using
expr : Vm
expr =
    ADD 
  & ADD
  & (PUSH1 $ Literal 0x01) 
  & NIL 


expr'' : Vm
expr'' = foldl (flip (&)) NIL 
  [ 
  (PUSH1 $ Literal 0x01), 
  ADD, 
  ADD
  ]

{-
-- we can't use fold because it's not a list...

-- OK - HANG ON - I THINK WE"VE GOT IT WRONG - 
-- the head should be the first operation....
-- and printing should reflect that.

mappend : Vm -> Vm -> Vm 
mappend c c1 = foldl (\x,y => (&) y x) c c1 
-}


-- if we're going to concat insns around then we will a concat

-- 
eval : Vm -> Integer
eval NIL = 0
eval ((&) ADD e) = eval e
eval ((&) SUB e) = eval e


main : IO ()
main = do
  printLn .human $ expr


  printLn .human $ expr''



-- (&) : Vm -> OpCode -> Vm
-- (&) = DOT
-- DOT : (Vm -> OpCode -> Nat ) ->  OpCode -> Vm -> Vm


