
{-
data Elem : a -> Vect k a -> Type where
  Here : Elem x (x :: xs) 
  There : (later : Elem x xs) -> Elem x (y :: xs) 
-}

{-
  actually if we model the stack depth...
  a literal just adds one to the stack depth. 
  can add subtracts one. eg. i but that
-}

-- change Elem to Expr

data Expr = 
  Literal Nat
  | Add Expr Expr



expr : Expr 
expr = Add (Literal 1) (Literal 2)



f : Expr -> Bool
f x = case x of
        Literal n => True
        Add lhs rhs => True 



-- so we can evaluate it. or print out op-codes etc, ... - 

main : IO ()
main = printLn "hithere"


