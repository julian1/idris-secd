
  
data T : Type where
  Nil :  T
  C : Integer -> T -> T
  L : T -> T -> T       

-- this works. it's possible a Nil can appear anywhere not just rhs. but that's equivalent to [ 123, [], 456 ]
-- so this works. But we have different data constructors for C versus L


Show T where
  show (Nil ) = "Nil"
  show (C val xs) = "C " ++ show val ++ " " ++ show xs
  show (L l xs) = "(" ++ show l ++ ")" ++ "(" ++ show xs ++ ")" 




k : T
k =  C 123  $ C 777  $ L ( C 456 $ C 123 Nil ) $ C 555 Nil

-- ugghhh - the problem is that we can't return the simple value...

-- TODO return Maybe Just...
index : Nat -> T -> T 
index n     Nil      = Nil
index Z     (C x xs) = x
index Z     (L l xs) = x

index (S i) (C x xs) = index i xs
index (S i) (L x xs) = index i xs



main : IO ()
main = do
  putStrLn $ show k
   
{-
data L : Type where
  Nil :  L 
  (::) : Item -> L -> L 


mutual


data Item :  Type where

  C : Integer -> Item   

  -- L : List (Item a) -> Item a
  -- L : L -> Item 
  -- L : L -> Item
  
  X : L ->  Item

  -- C : Integer -> L 

j : L 
j = C 456 :: C 123 :: Nil 

-}
