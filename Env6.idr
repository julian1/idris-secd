
data Item : Type where
  Nil   : Item
  C     : Integer -> Item
  L     : Item -> Item
  (::)  : Item -> Item -> Item     -- we want to constrain 1st Item to either C or L



Show Item where
  show (Nil ) = "Nil"
  show (C val) = "C " ++ show val
  show (L xs) = "(" ++ show xs ++ ")"
  show (x :: xs) =  show x ++ ", " ++ show xs


-- TODO return Maybe Just, instead of Nil if can't find 
-- indexed from 0
index : Nat -> Item -> Item
index i Nil = Nil                     -- bad, should be Nothing? Actually depends...
index Z     a = a 
index (S i) (x :: xs) = index i xs
index i (L  xs ) = index i xs


-- drill into structure using path
locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path



main : IO ()
main = do

  -- e = ((1 3) (4 (5 6))).
  let e =  L ( C 1 :: C 3 ) :: L ( C 4 :: L ( C 5 :: C 6 ) )  
  putStrLn $ show e

  putStrLn "-----"
  putStrLn $ show $ locate [ 1, 1 ] e    -- ok (C 5, C 6)    or should it be L (C 5, C 6)
  putStrLn $ show $ locate [ 1, 1, 1 ] e    -- ok C 6    
  putStrLn $ show $ locate [ 1, 1, 0 ] e    -- expect C 5   , got  (C 5, C 6) 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect C 1      , got (C 1, C 3), (C 4, (C 5, C 6))  


