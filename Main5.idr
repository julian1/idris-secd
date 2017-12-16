
{-
  OK - this is the approach - same as a haskell string.
    cons makes sense. ++ makes sense. we get fold etc. 

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

-- possible infix operators :+-*\/=.?|&><!@$%^~#
infixr 7 &



-- synonym for (::) refining the type makes it easier for the typechecker 
-- The confusing bit is nil is last... but it's the same as string concatenation
(&) :  OpCode -> List OpCode -> List OpCode
(&) =  Prelude.List.(::)

(^) :  OpCode -> List OpCode -> List OpCode
(^) =  Prelude.List.(::)


-- think all this stuff is wrong...
human'' : List OpCode -> String
human'' ops = foldl f "" ops where
  f acc op = acc ++ ", " ++ human op



main : IO ()
main = do
  printLn "hi"

  -- we can use the :: operator if we prefer for lists...
  let j = the (List OpCode) $ [ (PUSH1 $ Literal 0x01) , ADD  ]  -- this is the correct order.

  -- Ok, this works...
  let k =
      PUSH1 (Literal 0x01)
    & PUSH1 (Literal 0x02)
    & ADD
    & Nil

  printLn $ human' j
  printLn $ human' k
  printLn $ human' (k ++ k)

  pure ()


