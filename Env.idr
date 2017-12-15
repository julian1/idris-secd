{-
  Environment for SECD machine.

  - WE WANT a NIL to be able to be placed anywhere. allows NIL or LIst or sublist.
  but it would be nice if could enforce it at the end of a list...

  - THE BIG CONFUSION is thinking that (::) is the middle due to syntax, rather than a tag at the lhs/top of the expr...
  - ALSO VERY IMPORTANT (::) is *not* concat. or mappend.  it's just ordinary cons 
-}

module Env

-- https://github.com/jfdm/idris-testing/
import Test.Assertions
import Test.Utils


%access public export

data Item : Type where
  (::)  : Item -> Item -> Item
  C     : Integer -> Item
  L     : Item -> Item
  NIL   : Item


Show Item where
  show ((::) x y)  = show x ++ ", " ++ show y
  show NIL       = "NIL"
  show (C val)   = "C " ++ show val
  show (L val)   = "(L " ++ show val ++ ")"


Eq Item where
  (==) (x :: xs) (x' :: xs') = (==) x x' && (==) xs xs'
  (==) (L x) (L x') = (==) x x'
  (==) (C x) (C x') = x == x'
  (==) NIL   NIL = True
  (==) _ _ = False




index : Nat -> Item -> Item
index (S i) ((::) _  xs)    = index i xs
index Z     ((::) (L x) _)  = x  
index Z     (L x)           = x  
index Z     x               = x 


-- drill down into env
locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path


-- Tests
%access private


j : Item
j = (C 123) :: (C 456) 

l : Item
l = (C 123) :: NIL  

k : Item 
k = C 123 :: (L $ NIL :: NIL ) :: C 789

m : Item 
m = C 123 :: (L $ NIL :: C 123 ) :: C 789



-- This can be called with Env.runTests even though it's private
runTests: IO ()
runTests = do

  assertEquals k k 
  assertNotEquals k m 

  assertEquals (index 0 k) k 
  assertEquals (index 1 k) $ NIL :: NIL
  assertEquals (index 2 k) $ C 789

  assertEquals (locate [0] k) k 
  assertEquals (locate [1,0] k) $ NIL :: NIL
  assertEquals (locate [1,1] k) $ NIL 


  -- destructuring works 
  assertEquals 
    (case k of (C a :: (L $ NIL :: b  ) :: C c ) => (a,b,c) )
    (123, NIL, 789) 

  pure ()

