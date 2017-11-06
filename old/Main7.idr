
    sub1 : Nat -> Nat
    sub1 (S k) = k

    data Expr : (depth : Nat) -> Type where
      Literal : Integer -> Expr 1
      Add : Expr (S x) -> Expr (S y) -> Expr (x + y + 1)

    expr : Expr 1
    expr =
      Add (Literal 123) (Literal 456)




