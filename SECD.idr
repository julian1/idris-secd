
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

--  are stack items tagged? - or can the code/types keep track of it? on the stack algebraically - eg. know what it is... 



data Code : Type where
  -- value ...
  Value : Integer ->  Code

  Identifier : Integer ->  Code    -- debrujn index
  -- app  - takes 2 values off the stack...
  Ap    : Code
  -- a closure is pushed onto the stack
  Abstraction : Code


Show Code where
  show (Value val) = "Value " ++ show val
  show (Identifier val) = "Identifier " ++ show val
{-
case c of
    Value val => "Value " ++ show val
    _ => "code" 
-}

-- would be trying to use the evm stack here
data S : Type where
  Literal : Integer -> S

Show S where
  show c = "s" 



eval : (  List Code, List S )  -> (  List Code, List S )
eval (c:: cs, ss) = 
  case c of
    Value val => (cs, Literal val :: ss)
    _  => (cs, ss)






  
main : IO ()
main = do
  putStrLn $ "hithere " ++ show [ (Value 123) ]

  let codes = [ Value 123 ] 

  let ret = eval (codes, [])

  printLn $ show ret 

  pure ()


