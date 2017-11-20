
import Debug.Trace

data Code : Type where

  -- stack operations
  NIL : Code
  LDC : Integer ->  Code            -- load from context
  LD  : Nat -> Nat -> Code          -- load i,j from environment
  LDF : Code

  AP : Code
  OP : String -> Code



Show Code where

  -- stack operations
  show NIL = "NIL"
  show (LDC val) = "LDC " ++ show val
  show (LD i j) = "LD " ++ show i ++ " " ++ show j 
  show LDF = "LDF ??" 

  show (AP ) = "AP" 
  show (OP op) = "OP " ++ op 




data Item : Type where

  Constant : Integer -> Item

  Nil : Item

  Function : Item


Show Item where
  show (Constant val) = "Constant " ++ show val
  show (Nil ) = "Nil"
  show (Function ) = "Function ??" 

{-
  https://webdocs.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html
  http://netzhansa.blogspot.com/2008/09/revisiting-secd-and-power-of-lisp-while.html

  http://skelet.ludost.net/sec/
-}

-- we need to write the tools the right way around 
-- s e c d

eval : (List Item, List (List Item), List Code)  -> (List Item, List (List Item), List Code)

eval (s, e, Nil ) = (s, e, Nil )                            -- no more c - finish


eval (s, e, LDC val:: c ) = eval (Constant val :: s, e, c ) -- load constant on stack

eval (s, e, OP op ::cs ) =
  let (Constant a :: Constant b :: s') = s 
      value = case op of
        "+" => a + b 
        "*" => a * b 
  in
  eval ( Constant value:: s', e, cs)




-- Does op really evaluate something?  shouldn't it be apply?

main : IO ()
main = do

  let codes = [ LDC 3, LDC 4, OP "+" ]
  putStrLn "hi"

  let ret = eval ([], [[]], codes )

  printLn $ show ret


 {-
 -- e is a list of sublists. so variabes - can be lists...
  
  A lambda function (lambda plist body) is compiled to 
       (LDF) || (body' || (RTN))
  where body' is the compiled code for body.

  Example. (lambda (x y) (+ x y)) is compiled to
      (LDF (LD (1.2) LD (1.1) + RTN))   -- the lambda code...

    RTN 
      actually it restores the calling invocation stack - on d.
      is like a code path return. i think not just leave result on the stack.

    Ok, we know LD + and RTN . BUt what does LDF do...
    and what is the precedence????
    - remember its code, 

  
    LDF   s e (LDF f.c) d            ->  ((f.e).s) e c d
      - so the code jump point just gets pushed.

    this suggests that the code, is placed on the stack with copy of environment?... 

    IMPORTANT - I think cons is pushing to the environment - so the lambda variables are there - not the stack. 

    so the code, (LD (1.2) LD (1.1) + RTN) is placed on the stack - in one stack slot... 
      REMEMBER it's not being applied yet.

    
    AP    ((f.e') v.s) e (AP.c) d    ->  NIL (v.e') f (s e c.d)

    - v - the value goes in a new environment - the old environment is pushed to the dump.
    - s - the stack 

    - OK it leaves NIl on stack because it's a fresh stack for new execution. 
      while the current s e c are pushed to d that makes sense.
     


  An identifier is compiled to (LD (i.j)) where (i.j) is an index into stack e.

  Example. ((lambda (x y) (+ x y)) 2 3) compiles to
      (NIL LDC 3 CONS LDC 2 CONS LDF (LD (1.2) LD (1.1) + RTN) AP)

  Ok the cons and nil. are creating a tuple on the stack,
  NIL LDC 3 CONS LDC 2 CONS  -> (3,4)

  ahhhh - maybe the LDF loads this (LD (1.2) LD (1.1) + RTN)   which is why it is in parenthesis

  LDF changes the code stack?


  so LD (1.2) is an identifier in the environment - so what is it storing? 

  -- it's using freaking cons ?

  - LD is load from environment (ahhh perhaps - the environment) ahhh

  - maybe ldf - means to load it into the environment or stack?  not from the environment.


  - what are the cons doing.

  there's only one AP here...
-}

