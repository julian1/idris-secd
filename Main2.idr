
import Assembler
import Expr

import Test.Assertions
import Test.Utils


main : IO ()
main = do


  let ops' = resolve . compile . L $ 
        mstore 0x00 0xeeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffee  -- store value to 0x00
      ^ log0   0x00 32    -- log value at 0x0
      ^ return 0x00 32    -- return value at 0x0    
      ^ Nil

  let len = length' ops'

  let code = machine' . resolve . compile . L $ 
      -- copy contract code and loader to memory 0, starting loader start
        codecopy                                                  
          0     -- addr                                          
          (sym (Symbol "loader_start")) -- pos
          (sym (Plus (Sub (Symbol "loader_finish") (Symbol "loader_start"))(Literal len)))    

      -- create the contract . note we could just dup the length
      ^ create                                                    
          0     -- eth value
          0     -- addr
          (sym (Plus (Sub (Symbol "loader_finish") (Symbol "loader_start"))(Literal len)))    

      -- call our contract, swapping in the address returned from create
      ^ call gas (asm [ DUP 6 ]) 0  0x0 0x0 0x0 0x0              

      -- clean up stack
      ^ asm [ POP, POP, STOP ]                                   

      -- loader
      ^ label "loader_start"
  
      -- copy contract code to mem pos 0, starting at end of loader
      ^ codecopy                                                  
          0
          (sym (Sub (Symbol "loader_finish") (Symbol "loader_start")))
          (sym (Literal len))
      -- return the mem addr and len for where to find the contract in the create
      ^ return                                                    
          0
          (sym (Literal len))
      ^ label "loader_finish"

      -- the actual contract code
      ^ asm ops'
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

