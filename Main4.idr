
import Assembler

infixl 7 &

(&) : List a -> a -> List a
(&) = flip (::)


-- OK - i think our human statement is wrong.

-- VERY IMPORTANT - the test is to be able to cons a single new insn on. 


-- but what about ++ operator. will that work... 

-- should the head of the list be the last instruction... yes.  and tail be the first.
-- Not sure - it shouldn't be reversed


-- left fold 
human'' : List OpCode -> String
human'' ops = foldl f "" $ map human ops
  where f op acc = acc ++ ", " ++ op



main : IO ()
main = do
  printLn "hi"

  -- we can use the :: operator if we prefer for lists...
  let j = the (List OpCode) $ [ (PUSH1 $ Literal 0x01) , ADD  ]  -- this is the correct order.

--  let k = the (List OpCode) $  (PUSH1 $ Literal 0x01) :: ADD   :: Nil -- this is correct but looks confusing

  
  let k = the (List OpCode) $ [ ADD, (PUSH1 $ Literal 0x01)  ]  -- when in a list it would look like this.

  -- I think this is the correct ordering...
  let l = the (List OpCode) $  Nil
      & (PUSH1 $ Literal 0x01)
      & (PUSH1 $ Literal 0x01)
      & ADD

  -- Eg. cons should append an additional insn to other insns. this is correct. 
  let m = SUB :: l
  
  let n = [ SUB ] ++ l  -- this looks a bit confusing but I think it's correct
                        -- it's the reverse of a string

{-
  -- but its the reverse of haskell strings,
  > 'a' : "pple"
  "apple"
-}

  -- let l = the (List OpCode) $  ( ( Nil & ADD ) & ADD )



  -- map (putStrLn . human) j
  -- foldmap (pure ) j

  printLn $ human' j
  printLn $ human' k
  printLn $ human' l
  printLn $ human' m  
  printLn $ human'' n  

  pure ()


