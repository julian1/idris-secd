
  
data T : Type where
  Nil :  T
  C : Integer -> T
  L : T -> T       

  (::) : T -> T -> T     -- we want to constrain 1st T to either C or L

-- this works. but possible a Nil can appear anywhere not just rhs. but that's equivalent to [ 123, [], 456 ] actually no.
-- C 123 :: Nil :: Nil is valid. without L which would indicate a list.
-- so this works. But we have different data constructors for C versus L

-- we don't actually even need Nil, except to deconstruct


Show T where
  show (Nil ) = "Nil"
  show (C val) = "C " ++ show val 
  show (L xs) = "(" ++ show xs ++ ")" 
  show (x :: xs) =  show x ++ ", " ++ show xs 


-- TODO return Maybe Just...
-- indexed from 0
index : Nat -> T -> T
index n Nil = Nil
index Z     (x :: xs) = x
index (S i) (x :: xs) = index i xs


k : T
k =  C 123 :: C 456 :: L ( C 666 :: C 777 :: Nil :: Nil ) ::  C 789 :: Nil --  
-- k =  C 123 :: C 456 :: L ( C 666 :: Nil :: Nil )  --  
-- k =  C 123 

-- in

main : IO ()
main = do
  putStrLn $ show $ index 1 k
  putStrLn $ show $ index 2 k
  putStrLn $ show $ index 3 k

