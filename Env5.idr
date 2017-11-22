{- 
  this works. but possible a Nil can appear anywhere not just rhs. but that's equivalent to [ 123, [], 456 ] actually no.
  C 123 :: Nil :: Nil is valid. Which doesn't make sense if we use L to indicate a list.
  so this works. But we have different data constructors for C versus L

  we don't actually even need Nil, except to deconstruct
-}

  
data T : Type where
  Nil :  T
  C : Integer -> T
  L : T -> T       
  (::) : T -> T -> T     -- we want to constrain 1st T to either C or L



Show T where
  show (Nil ) = "Nil"
  show (C val) = "C " ++ show val 
  show (L xs) = "(" ++ show xs ++ ")" 
  show (x :: xs) =  show x ++ ", " ++ show xs 


-- TODO return Maybe Just...
-- indexed from 0
index : Nat -> T -> T
index i Nil = Nil    -- bad
index Z     (x :: xs) = x
index (S i) (x :: xs) = index i xs
index i     (L xs) = index i xs


locate  : List Nat -> T -> T
locate path val = foldl (\v, i => index i v ) val path



k : T
k =  C 123 :: C 456 :: L ( C 666 :: C 777 :: Nil :: Nil ) ::  C 789 :: Nil --  



main : IO ()
main = do
  putStrLn $ show k
  putStrLn $ show $ index 2 k
  putStrLn $ show $ index 3 k
  printLn "---"
  putStrLn $ show $ locate [ 2, 0 ] k 
  putStrLn $ show $ locate [ 0 ] k 
  putStrLn $ show $ locate [ 3 ] k 
  putStrLn $ show $ locate [ 2 ] k 

  -- putStrLn $ show $ index 0 (index 2  k) 

