
import Assembler

infixl 7 &

(&) : List a -> a -> List a
(&) = flip (::) 
 

main : IO ()
main = do
  printLn "hi"

  -- we can use the :: operator if we prefer for lists...
  let j = the (List OpCode) $ ADD :: ADD :: Nil
  
  let j = the (List OpCode) $  Nil & ADD

  pure ()


