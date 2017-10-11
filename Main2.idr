
-- ok, don't knwo that we can even encode what we want...


data Elem : Nat -> Type where
  -- Add : Elem x -> Elem y -> Elem (x +y)
  -- Add : Elem -> Elem -> Elem 123 
  Literal : Nat -> {gas : Nat} -> Elem gas

elem1 : Elem 456
elem1 = Literal 456




{-
elem2 : Elem 123
elem2 = Add (Literal 1) (Literal 123)
-}




f : Elem n -> Bool
f x = case x of
        Literal val => True
        --Add' lhs rhs => true 

