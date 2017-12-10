
-- example using the opcodes and composition functions 
-- op codes and composition 

-- probably just need (&) 

import Assembler 

infixl 7 & 

data Vm : Type where
  Initial : Vm
  -- some stack representation

  (.) : Vm -> OpCode  -> Vm
  (&) : Vm -> OpCode  -> Vm



-- our transition function
-- need to change around - so that it's   (op a b)  not (a op  b) ?
f : Vm -> OpCode -> Vm
f vm ADD = vm  
f vm SUB = vm  
f vm (PUSH1 val) = vm


ret : Vm -> Vm



main : IO ()
main = do


  -- not sure why this doesn't work...
  -- let ret = (\a => f a ADD ) . (f Initial ADD)  

  -- precedence is around the wrong way???

  -- Ok this composes in the right direction....

  let ret = (Initial  & ADD & ADD ) 


  printLn "hi"


