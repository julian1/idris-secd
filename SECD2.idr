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



data Item : Type where
  Nil   : Item
  C     : Integer -> Item
  L     : Item -> Item
  (::)  : Item -> Item -> Item     -- we want to constrain 1st Item to either C or L



Show Item where
  show (Nil ) = "Nil"
  show (C val) = "C " ++ show val
  show (L xs) = "(" ++ show xs ++ ")"
  show (x :: xs) =  show x ++ ", " ++ show xs


-- TODO return Maybe Just, instead of Nil if can't find 
-- indexed from 0
index : Nat -> Item -> Item
index i Nil = Nil                     -- bad, should be Nothing? Actually depends...
-- index Z     (x :: xs) = x
-- index Z     (L xs) = L xs

-- index Z     a    = a 
-- index Z     ( x :: xs) = x 
index Z     a = a 

index (S i) (x :: xs) = index i xs
-- index (S i) (L  xs ) = index i xs
index i (L  xs ) = index i xs

-- perhaps 0 should match the list. 1 should match first item, 2 second item...

-- this is all hopeless...

-- drill into structure using path
locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path


-- we need to write the tools the right way around
-- s e c d

eval : (Item, Item, List Code)  -> (Item, Item, List Code)


eval (s, e, Nil) = (s, e, Nil )                       -- no more c - finish

eval (s, e, LDC val:: c) = eval (C val :: s, e, c )   -- load constant on stack

eval (s, e, (LD path ) :: c ) = eval ( locate path e :: s, e, c )   -- load env on stack


--eval (s, e, CAR :: c) = eval (C 123 :: s, e, c )      -- load constant on stack


-- Thus, the expression (car (cons x y)) evaluates to x, and (cdr (cons x y)) evaluates to y.

eval (s, e, OP op ::cs) =
  let (C a :: C b :: s') = s
      val = case op of
        "+" => a + b
        "*" => a * b
  in
  eval ( C val :: s', e, cs)


-- the car operation returns the first element of the list, while cdr returns the rest of the list. cons.
-- Does op really evaluate something?  shouldn't it be apply?
-- Thus, the expression (car (cons x y)) evaluates to x, and (cdr (cons x y)) evaluates to y.

-- VERY IMPORTANT car and cons are builtins.
-- Plus builtin functions +,  *, ATOM, CAR, CONS, EQ, etc.

main : IO ()
main = do

  let codes =  [ LDC 3, LDC 2, LDC 6, OP "+", OP "*" ]
  let ret = eval (Nil, Nil, codes )
  putStrLn $ show ret


  putStrLn "-----"
  -- e = ((1 3) (4 (5 6))).
  let e =  L ( C 1 :: C 3 ) :: L ( C 4 :: L ( C 5 :: C 6 ) )  
  putStrLn $ show e

  -- ugghhh it drilled into the value... 
  -- it's not working...
  -- let ret = eval (Nil, e, [ LD [ 1 ] ] )
  -- putStrLn $ show ret

  putStrLn "-----"
  -- putStrLn $ show $ locate [ 1, 1, 0 ] e
  putStrLn $ show $ locate [ 1, 1 ] e    -- ok (C 5, C 6)    or should it be L (C 5, C 6)
  putStrLn $ show $ locate [ 1, 1, 1 ] e    -- ok C 6    
  putStrLn $ show $ locate [ 1, 1, 0 ] e    -- expect C 5   , got  (C 5, C 6) 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect C 1      , got (C 1, C 3), (C 4, (C 5, C 6))  


  -- so it's not working properly... to drill down...


  -- Env : Integer -> Env

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

