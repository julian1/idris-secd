
-- OK. this example works well...

data Expr : Nat -> Type where
  Literal : Integer -> Expr 2


eval : Expr n -> { auto condition : GT n 1 } -> Integer
eval x = case x of
  Literal val => val


y : Integer
y = eval (Literal 123)

