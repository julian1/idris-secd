

-- %elim data List : (elem : Type) -> Type where
-- (::) : (x : elem) -> (xs : List elem) -> List elem 

data Env : a -> Type where

  NIL :  Env a
  (::) : a -> Env a -> Env a


  C : Env a -> Env a ->  Env a


j : Env Integer
j = 123 :: 456 :: NIL

-- ahhhh this is very perculiar. we've introduced 

-- Ahhh it doesn't work because it's not consing....
-- because . lhs of cons must be an integer...
-- and this can never resolve 

k : Env Integer
k =  678 :: NIL 


-- OK we can join like this... but we can't cons it
m : Env Integer
m = 123 :: (C j k )  

