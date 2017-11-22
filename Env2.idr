
-- ok this works...
-- so cons is not a function. instead it's a data constructor. 

-- if A 

data Env : a -> Type where

  NIL :  Env a
  C : a -> Env a            -- It's ok to have C because we will have functions F as well...

  (::) : Env a -> Env a -> Env a

  L : Env a -> Env a




j : Env Integer
j = C 123 :: C 456 :: NIL

k : Env Integer
k = L $ C 123 :: C 456 :: NIL

m : Env Integer
m = j :: k :: NIL


-- And it can represent our environment
-- let e =  ((1 3) (4 (5 6))) 
e : Env Integer
e = (C 1 :: C 3) :: (C 4 :: (C 5 :: C 6 :: C 7))


main : IO ()
main = do

  let u = 
      the Integer $
      case m of
        ((C h :: C y) :: g) => h
  
  putStrLn "hi"




