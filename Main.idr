
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
      add 5 6
      ^ Nil

  let len = length' ops'

  -- ok this is problematic...
  -- OK
  -- OK important - a symbol is a literal reference...
  -- so we should be able to handle this. without asm...

  let code = machine' . resolve . compile . L $ 
        codecopy                                                  -- copy contract code to memory 0, code pos 30, len 16
          0                                               
          (sym (Symbol "loader_start"))
          16                                                     -- ok, we want a symbol subtraction... 
      ^ create 0 0 16                                            -- create contract eth value 0, mem address 0, len 16
      ^ call gas (asm [ DUP 6 ]) 0  0x0 0x0 0x0 0x0              -- call contract, swapping in address that was returned
      ^ asm [ POP, POP, STOP ]                                   -- clean up args

      ^ label "loader_start"
      ^ asm [ PUSH1 $ Literal len, DUP 1, PUSH1 $ Literal 0x0B, PUSH1 $ Literal 0, CODECOPY, PUSH1 $ Literal 0, RETURN ] -- this is the loader
      ^ label "loader_finish"

      -- ^ asm [ PUSH1 $ Literal 2, PUSH1 $ Literal 3, ADD  ]    -- this is the contract code
      ^ asm ops'
      ^ Nil

  assertEquals code "6010601E600039601060006000f060006000600060006000855af1505000600580600B6000396000f36006600501" 

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

