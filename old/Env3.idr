{-
  ok, this kind of works. 
  we don't Nil as an explicit type - but we do have L Nil
  - i think this works...
-}

data Env : a -> Type where

  C : a -> Env a  

  L : List (Env a) -> Env a




j : Env Integer
j = L [ C 123 , C 456, L [ C 345 ]  ]


m : Env Integer
m = L Nil  
  
-- but we can't easily just cons onto it...
-- we want to just Cons onto the list, with either a Constant, or another list.


n : Env Integer
n = L [ C 213, m ] -- ugly 



main : IO ()
main = do


  let u = the Integer $ 
    case m of 
      L Nil => 123
   
  putStrLn "hi"




