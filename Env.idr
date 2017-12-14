-- t x t x t x
-- OK. including NIL makes it more complicate. we should have a different operator...
-- ahh - and we *do* want to have a nil anywhere
-- NIL can be the starting value

-- WE WANT a NIL to be able to be anywhere. allows NIL or LIst or sublist.
-- but it would be nice if could enforce it at the end of a list...
-- it's no

data Item : Type where
  (::)  : Item -> Item -> Item

  C     : Integer -> Item
  L     : Item -> Item
  NIL   : Item


j : Item
j = (C 123) :: (C 456) 


k : Item 
k = C 456 :: (L $ C 789) :: C 123 

l : Item
l = (C 123) :: NIL  


main : IO ()
main = do
  -- 
  let (C a :: (L $ C b ) :: C c ) = k 

  printLn $ "hi " ++ show b



-- this is a concern. - no a double NIL is correct... because one is the list for L and the other is the :: continuation

-- Ahhh it doesn't work because it's not consing....
-- because . lhs of cons must be an integer...
-- and this can never resolve 

-- VERY IMPORTANT (::) is *not* concat. or mappend.  if we want this we have to 
-- write it.

-- but we can't use (::) to append a sublist - because (::) only accepts an Integer argument...
-- So it has to be an Either Integer or an Item type... basically...

-- So - I think it has to be like a tree (T a b) 

{-
data X : Type where
-- this works but then we don't even need NIL... except to be able to match...
  L     : Item -> X 
  C     : Integer -> X 
mutual 
-}


