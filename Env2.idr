

data Env : a -> Type where

  NIL :  Env a
  C : a -> Env a

  (::) : Env a -> Env a -> Env a




j : Env Integer
j = C 123 :: C 456 :: NIL

k : Env Integer
k = C 123 :: C 456 :: NIL

m : Env Integer
m = j :: k :: j




main : IO ()
main = do

  let u = 
      the Integer $
      case m of
        (h :: g) => 123
  
  putStrLn "hi"




