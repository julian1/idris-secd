
-- https://en.wikipedia.org/wiki/SECD_machine
-- lambda expression -> reverse polish notation

-- I think application - takes two items from stack - the abstraction and the application argument...

-- For example, the expression F (G X) (a single list element) is changed to the list X:G:ap:F:ap.
-- that then becomes C

{-

Evaluation of C proceeds similarly to other RPN expressions. 
    If the first item in C is a value, it is pushed onto the stack S. 
    More exactly, if the item is an identifier, the value pushed onto the stack will be the binding for that identifier in the current environment E. 
      If the item is an abstraction, a closure is constructed to preserve the bindings of its free variables (which are in E), and it is this closure which       is pushed onto the stack.
-}
-- if C
--   if a value -> then push onto stack S
--   if identifier -> push binding from E on stack  (? bound lambda?) - which could be an abstraction
--   if abstraction - (lambda abstraction is a function) then closure created? and the closure is pushed onto stack
--
--   If the item is ap, two values are popped off the stack and the application done (first applied to second).
--   If the result of the application is a value, it is pushed onto the stack.

--  how does one push a closure onto the stack? ahhh. maybe it's an encoding for an opcode....

--  are stack items tagged? - or can the code/types keep track of it? on the stack algebraically - eg. know what it is...



data Code : Type where
  -- value ...
  Value : Integer ->  Code

  Identifier : Integer ->  Code    -- debrujn index - 
  -- app  - takes 2 values off the stack...
  Ap    : Code
  -- a closure is pushed onto the stack
  Abstraction : Code


Show Code where
  show (Value val) = "Value " ++ show val
  show (Identifier val) = "Identifier " ++ show val
  show (Ap ) = "Ap" 
  show (Abstraction) = "Abstraction" 



{-
-- would be trying to use the evm stack here
data S : Type where
  Literal : Integer -> S

Show S where
  show c = "s"
-}


eval : (  List Code, List Code )  -> (  List Code, List Code )

eval ( Nil , ss) = (Nil, ss)

eval (c:: cs, ss) =
  case c of
    Value val => eval (cs, Value val :: ss)

    Abstraction => eval (cs, Abstraction :: ss)

    Ap => 
      eval (
        let ( s1 :: s2 :: ss' ) = ss 
        in (cs, ss')
      )
    _  => (cs, ss)







main : IO ()
main = do
  -- putStrLn $ "hithere " ++ show [ (Value 123) ]

  -- let codes = [ Value 123, Abstraction, Ap ]
  let codes = [ Value 123, Value 456 ]

  let ret = eval (codes, [])

  printLn $ show ret


