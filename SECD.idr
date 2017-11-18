
-- https://en.wikipedia.org/wiki/SECD_machine
-- lambda expression -> reverse polish notation 

-- I think application - takes two items from stack - the abstraction and the application argument...

-- For example, the expression F (G X) (a single list element) is changed to the list X:G:ap:F:ap.
-- that then becomes C 
-- if C 
--   if a value -> then push onto stack S
--   if identifier -> push binding from E on stack  (? bound lambda?)
--   if abstraction - (lambda abstraction is a function) then closure created? and the closure is pushed onto stack
--   
--   If the item is ap, two values are popped off the stack and the application done (first applied to second). 
--   If the result of the application is a value, it is pushed onto the stack.

--  how does one push a closure onto the stack? ahhh. maybe it's an encoding for an opcode....

--  need to treat the items on the stack algebraically - eg. know what it is... 


data Code : Type where
  -- value ...
  Value : Integer ->  Code
  -- app  - takes 2 values off the stack...
  Ap    : Code
  -- a closure is pushed onto the stack
  Abstraction : Code


-- would be trying to use the evm stack here
data S : Type where
  Integer : Integer -> S


eval : (  List Code, List S )  -> (  List Code, List S )
eval (c:: cs, ss) = 
  case c of
    Value val => (cs, ss)
    _  => (cs, ss)


  
main : IO ()

main = do
  putStrLn "hithere" 



