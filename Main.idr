
{-
  This is just the same as 1 :: 2 :: Nil  and is fine.
  let ret = Nil Dot ADD Dot ADD 

  But why can't we match on (Dot). Perhaps there's something else needed...
-}
-- example using the opcodes and composition functions 
-- op codes and composition 

-- probably just need (Dot) 

import Assembler 

-- infixl (&) Dot 

-- this isn't a Vm it's a Vm and state transition. 
data Vm : Type where
  -- Nil or identity
  Nil : Vm
  -- some stack representation
  -- ok, this is now the composition function
  -- this is a type or a
  -- mapend...
  Dot : Vm -> OpCode -> Vm


-- what does this mean...
Dot vm ADD = vm
Dot vm SUB = vm
Dot vm (PUSH1 val) =  vm


human : Vm -> String
human Nil = "whoot" 
-- human vm  = case vm of
--              ((Dot) _ _ ) => "whoot" 
-- human ((Dot) vm1 ADD vm2) = "whoot" 



-- we should be able to compose something together that can be matched... 
-- and


main : IO ()
main = do

  -- the thing is - that this entire expression is evaluated...
  -- is that what we want? 
  -- let ret = (\x => x) Dot ADD Dot ADD 

  let ret = Nil Dot ADD Dot ADD 

  printLn "hi"



  -- not sure why this doesn't work...
  -- let ret = (\a => f a ADD ) . (f Nil ADD)  
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


