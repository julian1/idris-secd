{-
  This prevents creating invalid structure without Nil
  
  But we cannot just cons onto the list
  instead must use L (x :: xs )


-}

data Item : Type where
  C     : Integer -> Item
  L     : List Item -> Item

Show Item where
  show (C val)   = "C " ++ show val
  show (L val)   = show val


-- but what about index...

cons : Item -> Item -> Item 
cons x b = case b of
  L lst => L (x :: lst)
  C val => L $ [ x , C val   ] 


index : Nat -> Item -> Item
index (S i) (L $ x :: xs)  = 
  case List.index' i xs of
    Just val => val 

index Z     (L $ x :: xs)  = x
index Z     (C val)    = C val

locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path





main : IO ()
main = do

  let e = the Item $ L [  L [ C 1, C 3 ] , L [ C 4 , L [ C 5 , C 6 ] ]  ]  

  -- let j =  cons (C 1)  e 
  printLn e 
  putStrLn "-----"

  putStrLn $ show $ locate [ 0 ] e       -- L 1,3   -- correct 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect 1 
  putStrLn $ show $ locate [ 1 ] e       -- expect L (4, (5, 6))  -- correct 
  putStrLn $ show $ locate [ 1, 1 ] e     -- expect (C 5, C 6)    
  putStrLn $ show $ locate [ 1, 1, 1 ] e    -- ok C 6    
  putStrLn $ show $ locate [ 1, 1, 0 ] e    -- expect C 5
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect C 1


  putStrLn "-----"


