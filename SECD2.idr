
import Debug.Trace

data Code : Type where
  -- value ...
  LDC : Integer ->  Code

  -- a closure is pushed onto the stack
  -- abstraction implies - a single argument
  -- Abstraction : Code
  LDF : Code

  AP : Code
  OP : String -> Code



Show Code where
  show (LDC val) = "LDC " ++ show val
  show (LDF ) = "LDF ??" 
  show (AP ) = "AP" 
  show (OP op) = "OP " ++ op 




data Item : Type where

  Constant : Integer -> Item

  Function : Item


Show Item where
  show (Constant val) = "Constant " ++ show val
  show (Function ) = "Function ??" 




-- c s 
eval : (  List Code, List Item )  -> (  List Code, List Item )

eval ( Nil , ss) = (Nil, ss)

eval (c::cs, ss) =
  case c of
    LDC val => eval (cs, Constant val :: ss)

    OP  op  => eval $ 
      case (op,ss) of
        ("plus", Constant a :: Constant b :: ss') => eval (cs,  Constant (a + b) :: ss')

-- Does op really evaluate something?  shouldn't it be apply?

main : IO ()
main = do

  let codes = [ LDC 3, LDC 4, OP "plus" ]
  putStrLn "hi"

  let ret = eval (codes, [])

  printLn $ show ret


 
