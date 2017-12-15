
import Assembler

infixl 7 &

(&) : List a -> a -> List a
(&) = flip (::)


-- OK - i think our human statement is wrong.

-- VERY IMPORTANT - the test is to be able to cons a single new insn on. 


-- but what about ++ operator. will that work... 

-- should the head of the list be the last instruction... yes.  and tail be the first.
-- Not sure - it shouldn't be reversed

--

main : IO ()
main = do
  printLn "hi"

  -- we can use the :: operator if we prefer for lists...
  let j = the (List OpCode) $ [ (PUSH1 $ Literal 0x01) , ADD  ]  -- this is the correct order.

  let k = the (List OpCode) $  (PUSH1 $ Literal 0x01) :: ADD   :: Nil -- this is correct but looks confusing


  -- I think this is the correct ordering...
  let l = the (List OpCode) $  Nil
      & (PUSH1 $ Literal 0x01)
      & (PUSH1 $ Literal 0x01)
      & ADD

  -- Eg. append a single op on. this is correct. 
  let m = SUB :: l

  -- let l = the (List OpCode) $  ( ( Nil & ADD ) & ADD )



  -- map (putStrLn . human) j
  -- foldmap (pure ) j

  printLn $ human' j
  printLn $ human' k
  printLn $ human' l
  printLn $ human' m  

  pure ()


