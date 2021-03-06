{-
  OP is second argument. The same as monadic >>= has the contination function as second argument. 

  thing is that we have to implement monoid, foldable, etc. for what value.
   
  implement foldable - and monid then we should get concat, concatMap etc for free 
  https://github.com/idris-lang/Idris-dev/blob/master/libs/prelude/Prelude/Foldable.idr

-}
-- OK - it really should be possible to handle a secd environment this way also...
-- cleaner way to express - than using lists...
-- we can add typing constraints to the behavior of the VM 

import Assembler

infixl 7 &


-- 1. a structure - that can be destructured - and manipulated and sugar printed
-- 2. (maybe) an evaluation function - necessary
-- 3. can do type level checks

-- this isn't a Vm it's a Vm and state transition.

-- since the assembler opcodes are defined before here. we can restructure this
-- Vm could be renamed OpCodes... It's only a VM if its executed

-- THE ISSUE is tht the ordering feels weird. unless we reverse it before using..
-- cons x xs  is a lot more natural


data Vm : Type where
  NIL : Vm                    -- return ? call it start? or have a symbol opCode? or more like return
  (&) : Vm -> OpCode -> Vm    -- should this be around the other way?
                              -- because that would be normal for a cons operator...


human : Vm -> String
human NIL        = "nil"
human ((&) xs op) = human xs ++ ", " ++ human op


expr : Vm
expr = 
  NIL 
  & (PUSH1 $ Literal 0x01) 
  & ADD 
  & ADD


-- we should be able to do append as a fold
{-
mappend : Vm -> Vm -> Vm 
mappend c NIL = c
mappend NIL c = c
mappend x ((&) e' op') = x & e'
-}


-- OK - we cant use fold because we don't have it defined for (&) operator...
-- append : Vm -> Vm -> Vm
-- append x y = foldl (flip (&)) x y
 
-- if we're going to concat insns around then we will a concat

-- 
eval : Vm -> Integer
eval NIL = 0
eval ((&) e ADD) = eval e
eval ((&) e SUB) = eval e




main : IO ()
main = do
  printLn .human $ expr

  -- we can use the :: operator if we prefer for lists...
  let j = the (List OpCode) $ ADD :: ADD :: Nil

-- (&) : Vm -> OpCode -> Vm
-- (&) = DOT
-- DOT : (Vm -> OpCode -> Nat ) ->  OpCode -> Vm -> Vm

  pure ()


