
import Assembler
import Expr

import Test.Assertions
import Test.Utils


main : IO ()
main = do

  let code = machine' . resolve . compile . L $ 
      ifelse 1 (1 + 5) (2 + 2)
      ^ Nil

  let code = machine' . resolve . compile . L $ 
        mstore 0x00 0xeeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffee  -- store value to 0x00
      ^ log0   0x00 32    -- log value at 0x0
      ^ return 0x00 32    -- return value at 0x0    
      ^ Nil

  -- works - sends 1 eth to address 0x, use hevm with --value flag.

  let code = machine' . resolve . compile . L $ 
      balance address                     -- show eth amount
      ^ asm [ POP ]                       
      ^ call gas 0x0 1 0x0 0x0 0x0 0x0    -- send 1 eth to 0x000000 
      ^ asm [ POP ]                       
      ^ balance address 
      ^ asm [ POP ]                       -- show eth amount
      ^ asm [ STOP ]
      ^ Nil


  -- we have to compile this seperately to resolve symbols locally and 
  -- to know the length
  let ops' = resolve . compile . L $
      -- add (add 5 6) 7
      add 5 6
      ^ Nil

  let len = length' ops'

  let code = machine' . resolve . compile . L $ 
      -- copy contract code and loader to memory 0, starting loader start
        codecopy                                                  
          0     -- addr                                          
          (sym (Symbol "loader_start")) -- pos
          (sym (Plus (Sub (Symbol "loader_finish") (Symbol "loader_start"))(Literal len)))    

      -- create contract. note we could just dup the length, from previous op
      ^ create                                                    
          0     -- eth value
          0     -- addr
          (sym (Plus (Sub (Symbol "loader_finish") (Symbol "loader_start"))(Literal len)))    

      -- call our contract, swapping in the address returned from create
      ^ call gas (asm [ DUP 6 ]) 0  0x0 0x0 0x0 0x0              

      -- clean up stack
      ^ asm [ POP, POP, STOP ]                                   

      -- loader- copy contract code to mem pos 0, starting at end of loader
      ^ label "loader_start"
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



--  assertEquals code "6010601E600039601060006000f060006000600060006000855af1505000600580600B6000396000f36006600501" 
  assertEquals code "6011601E600039601160006000f060006000600060006000855af15050006005600C60003960056000f36006600501"

  -- OK I think we need to be able to expose the labels, actually can just do it with asm code.
  -- importantly - we need to be able to compile code in separate label space... or treat the code as data...


  printLn code

  h <- fopen "out.vm" "w" 
  case h of 
    Right f => do
      printLn "whoot"
      fPutStr f code
      closeFile f
      pure ()


  pure ()

