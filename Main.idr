
{-
  This is just the same as 1 :: 2 :: Nil  and is fine.
  let ret = Nil & ADD & ADD 

  But why can't we match on (&). Perhaps there's something else needed...
-}
-- example using the opcodes and composition functions 
-- op codes and composition 

-- probably just need (&) 

import Assembler 

infixl 7 & 

-- this isn't a Vm it's a Vm and state transition. 
data Vm : Type where
  -- Nil or identity
  Nil : Vm
  -- some stack representation
  -- ok, this is now the composition function
  -- this is a type or a
  -- mapend...
  (&) : Vm -> OpCode -> Vm


-- what does this mean...
(&) vm ADD = vm
(&) vm SUB = vm
(&) vm (PUSH1 val) =  vm


human : Vm -> String
human Nil = "whoot" 
-- human vm  = case vm of
--              ((&) _ _ ) => "whoot" 
-- human ((&) vm1 ADD vm2) = "whoot" 



-- we should be able to compose something together that can be matched... 
-- and


main : IO ()
main = do

  -- the thing is - that this entire expression is evaluated...
  -- is that what we want? 
  -- let ret = (\x => x) & ADD & ADD 

  let ret = Nil & ADD & ADD 

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


