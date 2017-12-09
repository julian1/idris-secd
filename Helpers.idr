
module Helpers


import Debug.Trace

-- rename integerFromNat ?
natToInteger : Nat -> Integer
natToInteger = fromInteger . toIntegerNat


-- Stuff should be moved to Utility...

-- runtime assertion...
-- usage> :exec myassert "whoot" (>10) 12
myassert : String -> (a -> Bool) -> a -> a
myassert message p x =
  case p x of
    True => x
    False => trace message x


-- destructure an integer into byte list - should change to Byte8?
bytes : Integer -> List Integer
bytes x =
  let (_,xs) = f (x,[])
  in xs
  where
    f : (Integer, List Integer) -> (Integer, List Integer)
    f (0,acc) = (0, acc)
    f (x,acc) = f $ (div x 256, mod x 256 :: acc)


-- left pad a list with element
lpad : Nat -> a -> List a -> List a
lpad n x xs =
  replicate (n `minus` length xs) x  ++ xs


-- hex value of a Integer number - with left padding
export
showHex : Nat -> Integer -> String
showHex w x =
  -- we should detect if the number overfills width ...
  -- TODO use haskell Data.(&) equivalent instead...
  let b = bytes x in
  let b' = lpad w 0 b in
  let b'' = myassert "ERROR: integer exceeds width" (\b => length b <= w) b' in
  foldr f "" $ b''
  where
    f x acc = (b8ToHexString .fromInteger) x ++ acc

