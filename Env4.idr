
  
 data T : Type where

  Nil :  T
  -- (::) : Item -> L -> L 
  -- L : Integer -> T

  R : Integer -> T -> T

  L : T -> T -> T


j : T
j =  R 456 $ R 123 Nil 


k : T
k =  L ( R 456 $ R 123 Nil ) Nil

   
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
