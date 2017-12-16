
import Assembler
import Expr


main : IO ()
main = do

  let expr = ifelse 1 (1 + 5) (2 + 2)

  let code = machine' . resolve . compile $ expr
  
  printLn code

  h <- fopen "out.vm" "w" 
  case h of 
    Right f => do
      printLn "whoot"
      fPutStr f code
      closeFile f
      pure ()


  pure ()

