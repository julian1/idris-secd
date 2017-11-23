{-
  this works well . Except that we can't just cons the damn thing.... 
    when working with the stack of the secd.

  and we can't just return the damn item - C or T
-}
data Item : Type where
  Nil   : Item

  C     : Integer -> Item
  T     : Item -> Item
  (::)  : Item -> Item -> Item


-- can change this to take an Integer only
-- Or use T as a T constructor

Show Item where
  show (x :: xs) =  show x ++ ", " ++ show xs
  show Nil       = "Nil"
  show (C val)   = "C " ++ show val
  show (T val)   = "T (" ++ show val ++ ")"



-- TODO return Maybe Just, instead of Nil if can't find 
-- indexed from 0
index : Nat -> Item -> Item
index (S i) (x :: xs)  = index i xs
index Z     (x :: xs)  = x
index Z     Nil        = Nil 

-- drill into structure using path
locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path



main : IO ()
main = do

  -- e = ((1 3) (4 (5 6))).
  -- let e =  T [ 1, 3 ]  $  T ( 4 :: T [ 5, 6 ] Nil ) Nil 
  
  -- let e =  the Item $ T ( C 1 :: C 3 :: Nil ) :: T ( C 4 :: T ( C 5 :: C 6 :: Nil ) :: Nil ) :: Nil 
  let e =  the Item $ T [ C 1, C 3 ] :: T [ C 4 , T [ C 5 , C 6 ] ]  :: Nil 


  putStrLn $ show e
  putStrLn "-----"


{-
  putStrLn $ show $ locate [ 0 ] e       -- 1,3   -- correct 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect 1 
  putStrLn $ show $ locate [ 1 ] e       -- expect (4, (5, 6))  -- correct 
  putStrLn $ show $ locate [ 1, 1 ] e     -- expect (C 5, C 6)    
  putStrLn $ show $ locate [ 1, 1, 1 ] e    -- ok C 6    
  putStrLn $ show $ locate [ 1, 1, 0 ] e    -- expect C 5   , got  (C 5, C 6) 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect C 1      , got (C 1, C 3), (C 4, (C 5, C 6))  
-}
