
-- creating an integer is pretty simple
-- can do
-- the MyInteger 123


data MyInteger =
  Z | 
  S MyInteger 

-- data MyInteger :  where

-- myadd : MyInteger -> MyInteger -> MyInteger 

total myplus : (n, m : MyInteger ) -> MyInteger 
myplus Z right        = right
myplus (S left) right = S (myplus left right)



||| Convert an Integer to a Nat, mapping negative numbers to 0
fromIntegerNat : Integer -> MyInteger 
fromIntegerNat 0 = Z 
fromIntegerNat n = 
  if (n > 0) then
    S (fromIntegerNat (assert_smaller n (n - 1)))
  else
    Z  

Num MyInteger where
    (+) = myplus 
    (*) = myplus 

    fromInteger = fromIntegerNat



