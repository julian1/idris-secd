{-
  SECD

  https://webdocs.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html

  http://netzhansa.blogspot.com/2008/09/revisiting-secd-and-power-of-lisp-while.html
  http://skelet.ludost.net/sec/
-}

import Debug.Trace

data Code : Type where

  -- stack operations
  NIL : Code
  LDC : Integer ->  Code            -- load from context


  LD  : List Nat -> Code          -- load i,j from environment. might be i,j,k,l etc.
                                    -- can actually load a list on the stack. eg. more than one element.
                                    -- Ahhh but might be able to be done efficiently by just linking to it.
                                    -- similar to push jump address.
  LDF : Code

  CAR : Code
  CDR : Code

  AP : Code
  OP : String -> Code



Show Code where

  -- stack operations
  show NIL = "NIL"
  show (LDC val) = "LDC " ++ show val
  show (LD xs) = "LD " ++ show xs -- eg. i,j -- i ++ " " ++ show j
  show LDF = "LDF ??"

  show CAR = "CAR"
  show CDR = "CDR"

  show (AP ) = "AP"
  show (OP op) = "OP " ++ op


--- env

{-
data Item : Type where
  C     : Integer -> Item
  L     : List Item -> Item

Show Item where
  show (C val)   = "C " ++ show val
  show (L val)   = show val
-}



data Item : Type where
  (::)  : Item -> Item -> Item
  C     : Integer -> Item         -- change to Nat? 
  L     : Item -> Item
  NIL   : Item


Show Item where
  show ((::) x y)  = show x ++ ", " ++ show y
  show NIL       = "NIL"
  show (C val)   = "C " ++ show val
  show (L val)   = "(L " ++ show val ++ ")"



-- env operations

index : Nat -> Item -> Item
index (S i) (L $ x :: xs)  = 
  case List.index' i xs of
    Just val => val 
    -- Nothing...

index Z  (L $ x :: xs)  = x
index Z  (C val)        = C val

locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path



-- this whole destructuring thing - needing L is not very nice 
-- it would be so much nicer if we could work without the L ...


-- s e c d
eval : (Item, Item, List Code)  -> (Item, Item, List Code)

eval (s, e, Nil) = (s, e, Nil )                       -- no more c - finish

eval (L s, e, LDC val:: c) = eval ( L $ C val :: s, e, c )   -- load constant on stack

eval (L s, e, (LD path ) :: c ) =  eval ( L $ locate path e :: s, e, c )   -- load env on stack


eval (L (C a :: C b :: s), e, OP op :: c) = 
  let ret = case op of 
        "+" => a + b
        "*" => a * b
  in eval ( L $ C ret :: s, e, c)    

eval (L $ (L $ x :: xs) :: s, e, CAR :: c) = eval (L $ x :: s, e, c )   -- car




-- the car operation returns the first element of the list, while cdr returns the rest of the list. cons.
-- Does op really evaluate something?  shouldn't it be apply?
-- Thus, the expression (car (cons x y)) evaluates to x, and (cdr (cons x y)) evaluates to y.

-- VERY IMPORTANT car and cons are builtins.
-- Plus builtin functions +,  *, ATOM, CAR, CONS, EQ, etc.

main : IO ()
main = do

  -- Example 1. 
  let codes =  [ LDC 3, LDC 2, LDC 6, OP "+", OP "*" ]
  let ret = eval ( L Nil, L Nil, codes )
  putStrLn $ show ret


  putStrLn "-----"

  -- e = ((1 3) (4 (5 6))).
  let e =  the Item $ L [ L [ C 1, C 3 ] , L [ C 4 , L [ C 5 , C 6 ] ]  ] 
  putStrLn $ show e


  putStrLn "-----"
  putStrLn $ show $ locate [ 1, 1, 0 ] e  -- C5
  putStrLn $ show $ locate [ 1, 1 ] e    -- ok (C 5, C 6)    or should it be L (C 5, C 6)
  putStrLn $ show $ locate [ 1, 1, 1 ] e    -- ok C 6    
  putStrLn $ show $ locate [ 1, 1, 0 ] e    -- expect C 5   , got  (C 5, C 6) 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect C 1      , got (C 1, C 3), (C 4, (C 5, C 6))  


  putStrLn "-----"

  -- Example 2
  let codes =  [ LD [ 1, 1], CAR , LD [ 0, 0 ], OP "+" ]
  let ret = eval ( L Nil, e, codes )
  putStrLn $ show ret


 

{-
iNum Integer where
    (+) = prim__addBigInt
    (*) = prim__mulBigInt

-- interface with Number ...
Num Env where
   (+)   C a C b = prim__addBigInt a b
    (*) = prim__mulBigInt
    fromInteger n = C n
-}


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

