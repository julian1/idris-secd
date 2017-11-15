

bytes : Integer -> List Integer
bytes x = 
  let (_,xs) = f (x,[]) 
  in xs where
    f : (Integer, List Integer) -> (Integer, List Integer) 
    f (0,acc) = (0, acc)
    f (x,acc) = f $ (div x 256, mod x 256 :: acc)



showHex : Integer -> String
showHex x = 
  foldr f "" $ bytes x 
  where
    f x acc = (b8ToHexString .fromInteger) x ++ acc 




--- hmm a string can't be concatted ... actually probably can 
-- what do we need  - basically right shift 
-- an increasing number...

-- val - (val % 0xff)


-- div 255 (pow 0x2 0x8)
-- 0 : Integer
{-
f : Integer -> Integer 
f x = mod x (0xff + 1)

g : Integer -> Integer 
g x = div x (0xff + 1)
-}


-- j : (Integer,Integer) -> (Integer,Integer) 
-- j (x,d) = (f x, g x)



-- it should be left shifting the value down...

{-

  let h = pow 2 $ fromInteger d in
  -- div 0xffffff ( 0xff + 1)


j : Integer -> Nat -> Integer 
j x i = pow x i 

-- we just need to modulo the dam value...

u : Integer -> Nat -> Integer
u x i = 
  let m = pow 2 i in

  x - (  (div x m) * m) 
-}
