{-
  This prevents us creating an invalid structure without Nil on rhs, or with Nil somewhere else
  
  But we have to write our own cons'ing operator
-}

data Item : Type where
  C     : Integer -> Item
  L     : List Item -> Item

Show Item where
  show (C val)   = "C " ++ show val
  show (L val)   = show val


-- so have to have a special cons operator
cons : Item -> Item -> Item 
cons x xs = case xs of
  L lst => L (x :: lst)
  C val => L $ [ x , C val ] -- may not even need 


(::) : Item -> Item -> Item 
(::) = cons


index : Nat -> Item -> Item
index (S i) (L $ x :: xs)  = 
  case List.index' i xs of
    Just val => val 

index Z  (L $ x :: xs)  = x
index Z  (C val)        = C val

locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path





main : IO ()
main = do

  let e = the Item $ L [  L [ C 1, C 3 ] , L [ C 4 , L [ C 5 , C 6 ] ]  ]  
  printLn e 
  putStrLn "-----"


  printLn $ the Item $ C 1 :: C 2 :: e 
  printLn $ the Item $ C 1 :: C 2 :: L Nil  
  putStrLn "-----"


  putStrLn $ show $ locate [ 0 ] e       -- L 1,3   -- correct 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect 1 
  putStrLn $ show $ locate [ 1 ] e       -- expect L (4, (5, 6))  -- correct 
  putStrLn $ show $ locate [ 1, 1 ] e     -- expect (C 5, C 6)    
  putStrLn $ show $ locate [ 1, 1, 1 ] e    -- ok C 6    
  putStrLn $ show $ locate [ 1, 1, 0 ] e    -- expect C 5
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect C 1


  putStrLn "-----"


