

data Code : Type where
  -- value ...
  LDC : Integer ->  Code

  -- a closure is pushed onto the stack
  -- abstraction implies - a single argument
  -- Abstraction : Code
  LDF : Code

  AP : Code



Show Code where
  show (LDC val) = "LDC " ++ show val
  show (LDF ) = "LDF ??" 
  show (AP ) = "AP" 




data Item : Type where

  Constant : Integer -> Item

  Function : Item




-- c s 
eval : (  List Code, List Item )  -> (  List Code, List Item )

eval ( Nil , ss) = (Nil, ss)

eval (c::cs, ss) =
  case c of
    LDC val => eval (cs, Constant val :: ss)



main : IO ()
main = do

  -- let codes = [ Value 123, Value 123 ]
  putStrLn "hi"

 
