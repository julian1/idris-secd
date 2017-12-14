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


Show Item where
  show ((::) x y)  = show x ++ ", " ++ show y
  show NIL       = "NIL"
  show (C val)   = "C " ++ show val
  show (L val)   = "(L " ++ show val ++ ")"


Eq Item where
  (==) (x :: xs) (x' :: xs') = (==) x x' && (==) xs xs'
  (==) (L x) (L x') = (==) x x'
  (==) (C x) (C x') = x == x'
  (==) NIL   NIL = True
  (==) _ _ = False



-- THE BIG CONFUSION is thinking that (::) is the middle, rather than the lhs/top...

index : Nat -> Item -> Item
index (S i) ((::) _  xs)    = index i xs
index Z     ((::) (L x) _)  = x  
index Z     (L x)           = x  
index Z     x               = x 


-- index Z     (L $ x :: _) = x  
-- index Z  (L x)      = x 
--index Z  (L $ x :: xs)         = x  --  not sure... this isn't being matched - because it's actuall L (::) 
-- where we drill into a list is weirddd

-- ok - this has to change to support drilling in...
locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path



-- index Z  (L $ x :: xs)  = x
--  case List.index' i xs of
--    Just val => val 
-- TODO Nothing... must handle
-- index Z  (C val)        = C val


j : Item
j = (C 123) :: (C 456) 

l : Item
l = (C 123) :: NIL  


k : Item 
k = C 123 :: (L $ NIL :: NIL ) :: C 789

m : Item 
m = C 123 :: (L $ NIL :: C 123 ) :: C 789




myassert : Item  -> Item  -> Nat
myassert x y = case x == y of
                  True => 1


main : IO ()
main = do
  -- destructuring works 
  -- let (C a :: (L $ C b :: d  ) :: C c ) = k 
  -- printLn $ "hi " ++ show b

  printLn k

  printLn $ k == m

  printLn $ "---" 
  printLn $ index 0 k 
  printLn $ index 1 k 
  printLn $ index 2 k 

  printLn $ "---" 
  printLn $ locate [1,0] k 

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


