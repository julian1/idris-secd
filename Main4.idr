
-- this is imposing a condition on the argument...
-- not a proof.

-- add two numbers - first arg is smaller than the second
add : (x : Nat) -> (y : Nat) -> {auto smaller' : LT x y} -> Nat
add x y = x + y


main : IO ()
main = print $ add 3 4


