
import Data.Vect
import Data.Fin

-- types in our language.
-- this is the type of any expression - an int, bool or function ->  
-- this would include lambdas.
data Ty = TyInt | TyBool | TyFun Ty Ty



-- translation to types in idris
-- thiis is a mapping - may only be needed during evaluation?
interpTy : Ty -> Type
interpTy TyInt       = Integer
interpTy TyBool      = Bool
interpTy (TyFun A T) = interpTy A -> interpTy T   -- T for Term

-- Eg. a function from int to int
-- interpTy (TyFun  TyInt TyInt)

-- the context means local variables
-- "local variables (the context)."

{-
> :let xs : Vect 2 Ty ; xs = [ TyInt, TyInt] ;
*Interpreter> xs
[TyInt, TyInt] : Vect 2 Ty

> :t HasType 1 xs TyInt
HasType 1 xs TyInt : Type

we can construct the type - meaning it's good?
no - we have to try and construct via one of the constructors...

-}

---
-- remember - all the typing with the context is happening at compile time.
-- G is context which is a Vect
-- using, is specifying G as type
using (G:Vect n Ty)

  -- it's a vector of Types - eg. Vect n Ty
  -- expression - indexed by context (local variables) and the type of the expression itself, 
  data Expr : Vect n Ty -> Ty -> Type

  --- hmmmn it's using Fin for laziness?
  --- the only thing using HasType is Var. So it's to manipulate the variable context

  -- HasType i G T, which is a proof that variable i in context G has type T. This is defined as follows:
  data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
      Stop : HasType FZ (t :: G) t
      Pop  : HasType k G t -> HasType (FS k) (u :: G) t


  data Expr : Vect n Ty -> Ty -> Type where

      Var : HasType i G t -> Expr G t

      Val : (x : Integer) -> Expr G TyInt

      Lam : Expr (a :: G) t -> Expr G (TyFun a t)

      App : Expr G (TyFun a t) -> Expr G a -> Expr G t

      Op  : (interpTy a -> interpTy b -> interpTy c) ->
            Expr G a -> Expr G b -> Expr G c

      If  : Expr G TyBool ->
            Lazy (Expr G a) ->
            Lazy (Expr G a) -> Expr G a

  -- Am not sure this ever actually will get created - instead it's just a vec of the types
  -- but it might be useful.
  context: Vect 2 Ty
  context = [ TyInt, TyInt]

  -- val is easy
  x : Expr G TyInt 
  x = Val 123 


  -- So this is really a full typed lambda expression...
  -- we may even be able to use variables for functions...
  -- but what about modelling gas etc...

  -- binary add expression...
  add : Expr G (TyFun TyInt (TyFun TyInt TyInt))
  add = Lam (Lam (Op (+) (Var Stop) (Var (Pop Stop))))


  inc' : Expr G (TyFun TyInt TyInt)
  inc' = Lam (Op (+) (Var Stop) (Val 123 ))

  -- So we have 

  -- OK - it somehow knows the number of args ....
  -- OK - it's the Lam expression that manipulates the args in G

  -- So we should be able to do an apply. and have expressions... 

  identity : Expr G (TyFun TyInt TyInt)
  identity = Lam (Var Stop) 


  -- App : Expr G (TyFun a t) -> Expr G a -> Expr G t
  -- applying a value to identity
  apply : Expr G TyInt
  apply = App identity  (Val 123)  

  apply2 : Expr G TyInt
  apply2 = App (App add (Val 123))  (Val 123)  


  -- OK how do we bring this stuff into scope.


  main : IO ()
  main =
    printLn "hi"

