
data Expr = 
  Literal Nat
  | Add Expr Expr


data Elem : Type where
  Literal : Integer -> Elem 
  Add : Elem -> Elem -> Elem 





