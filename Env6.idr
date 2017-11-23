
data Item : Type where
  Nil   : Item
  -- C     : Integer -> Item
  (::)  : Integer -> Item -> Item     -- we want to constrain 1st Item to either C or L
  L     : Item -> Item -> Item

-- can change this to take an Integer only
-- Or use L as a L constructor

Show Item where
  show (Nil ) = "Nil"
  show (val) =  show val
  show (L x xs) = "(" ++ show x ++ ", " ++ show xs ++ ")"
  show (x :: xs) =  show x ++ ", " ++ show xs


-- TODO return Maybe Just, instead of Nil if can't find 
-- indexed from 0
index : Nat -> Item -> Item

index Z     a = a 
index (S i) (x :: xs) = index i xs

index Z     (L  x xs ) = x 
index (S i) (L  x xs ) = index i xs 
{-
  case i of 
    Z => x  
    _ => index i xs 
-}

-- drill into structure using path
locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path



main : IO ()
main = do

  -- e = ((1 3) (4 (5 6))).
  -- let e =  L ( 1 :: 3 :: Nil ) :: L ( 4 :: L ( 5 :: 6 :: Nil ) :: Nil )  
  let e =  L ( 1 :: 3 :: Nil ) :: L ( 4 :: L ( 5 :: 6 :: Nil ) :: Nil )  
  let e =  1 :: 3 :: Nil 
  putStrLn $ show e

  putStrLn "-----"
  putStrLn $ show $ locate [ 1, 1 ] e    -- ok (C 5, C 6)    or should it be L (C 5, C 6)
  putStrLn $ show $ locate [ 1, 1, 1 ] e    -- ok C 6    
  putStrLn $ show $ locate [ 1, 1, 0 ] e    -- expect C 5   , got  (C 5, C 6) 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect C 1      , got (C 1, C 3), (C 4, (C 5, C 6))  


