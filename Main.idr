
import Assembler
import Expr


main : IO ()
main = do

  -- let expr = ifelse 1 (1 + 5) (2 + 2)
  let code = machine' . resolve . compile . L $ 
        mstore 0x00 0xeeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffee
      ^ log0   0x00 32
      ^ return 0x00 32
      ^ Nil

  printLn code

  h <- fopen "out.vm" "w" 
  case h of 
    Right f => do
      printLn "whoot"
      fPutStr f code
      closeFile f
      pure ()


  pure ()

