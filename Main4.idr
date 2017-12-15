
import Assembler

infixl 7 &

(&) : List a -> a -> List a
(&) = flip (::)
 

main : IO ()
main = do
  printLn "hi"

  -- we can use the :: operator if we prefer for lists...
  let j = the (List OpCode) $ [ (PUSH1 $ Literal 0x01) , ADD  ]  -- this is the correct order.

  let k = the (List OpCode) $  (PUSH1 $ Literal 0x01) :: ADD   :: Nil -- this is correct but looks confusing
  
  
  let l = the (List OpCode) $  Nil & (PUSH1 $ Literal 0x01) & ADD    -- looks ok but is incorrect
  


  -- map (putStrLn . human) j 
  -- foldmap (pure ) j 

  printLn $ human' j
  printLn $ human' k
  printLn $ human' l

  pure ()


