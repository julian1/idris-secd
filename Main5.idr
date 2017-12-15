
{-
if one wanted to represent a list of instructions for a stck machine/vm - what order would one choose? 

  [ PUSH, PUSH, ADD ] 
    The in-order for [] and ++/mappend notation. map in order of executtion   
    [ insn1, insn2 ] ++ [ insn3 ] good
    map is also good from first to last.
    same as how a strings are manipulated in haskell.

  [ ADD, PUSH, PUSH ] ?  
    cons :: makes more sense in terms of cons'ing a new instruction onto head of the last instructions or nil. 
-}

import Assembler

infixr 7 &

-- (&) : List a -> a -> List a
-- (&) = flip (::)

(&) :   OpCode -> List OpCode -> List OpCode 
(&) =  Prelude.List.(::)




main : IO ()
main = do
  printLn "hi"

  -- we can use the :: operator if we prefer for lists...
  let j = the (List OpCode) $ [ (PUSH1 $ Literal 0x01) , ADD  ]  -- this is the correct order.

{-  let k = the (List OpCode) $  Nil
      & (PUSH1 $ Literal 0x01)
      & (PUSH1 $ Literal 0x01)
      & ADD
-}

  -- let k = the (List OpCode) $  (Nil & (PUSH1 $ Literal 0x01) ) & ADD
  -- let k = the (List OpCode) $  ( ADD  &  ((PUSH1 $ Literal 0x01) & Nil)) 
  let k = the (List OpCode) $  (PUSH1 $ Literal 0x01) & ADD  & Nil

  printLn $ human' j

  printLn $ human' k

  pure ()


