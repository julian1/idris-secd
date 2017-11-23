{-
    problem here, is that can put Nil anywhere... 
    and can construct without it being the terminating elt. 
-}
data Item : Type where
  Nil   : Item        -- having this allows us to use ordinar [] syntax 
  (::)  : Item -> Item -> Item

  C     : Integer -> Item
  L     : Item -> Item


-- can change this to take an Integer only
-- Or use L as a L constructor

Show Item where
  show (x :: xs) =  show x ++ ", " ++ show xs
  show Nil       = "Nil"
  show (C val)   = "C " ++ show val
  -- show (L val)   = "(L " ++ show val ++ ")"
  show (L val)   = "(" ++ show val ++ ")"



-- LODO return Maybe Just, instead of Nil if can't find 
-- indexed from 0
index : Nat -> Item -> Item
index (S i) (x :: xs)  = index i xs
index Z     (x :: xs)  = x
index Z     (C val)    = C val
index Z     Nil        = Nil 
index i     (L val)    = index i val

-- drill into structure using path
locate : List Nat -> Item -> Item
locate path val = foldl (flip index) val path


{-
check : Item -> Item -> IO ()
check a b = do 
  case a == b of
    True => printLn "ok"
 -}   

main : IO ()
main = do

  -- e = ((1 3) (4 (5 6))).
  -- let e =  L [ 1, 3 ]  $  L ( 4 :: L [ 5, 6 ] Nil ) Nil 
  
  -- let e =  the Item $ L ( C 1 :: C 3 :: Nil ) :: L ( C 4 :: L ( C 5 :: C 6 :: Nil ) :: Nil ) :: Nil 
  let e =  the Item $ [ L [ C 1, C 3 ] , L [ C 4 , L [ C 5 , C 6 ] ]  ] 


  putStrLn $ show e
  putStrLn "-----"

--  check (locate [ 0 ] e)  ( L [ C1, C3 ])


  putStrLn $ show $ locate [ 0 ] e       -- L 1,3   -- correct 
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect 1 
  putStrLn $ show $ locate [ 1 ] e       -- expect L (4, (5, 6))  -- correct 
  putStrLn $ show $ locate [ 1, 1 ] e     -- expect (C 5, C 6)    
  putStrLn $ show $ locate [ 1, 1, 1 ] e    -- ok C 6    
  putStrLn $ show $ locate [ 1, 1, 0 ] e    -- expect C 5
  putStrLn $ show $ locate [ 0, 0 ] e    -- expect C 1
{-
-}
