
import Assembler
import Expr


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



  let len = 5

  let code = machine' . resolve . compile . L $ 
        codecopy 0 30 16                                    -- copy contract code to memory 0, code pos 30, len 16
      ^ create 0 0 16                                   -- create contract value 0, mem address 0, len 16
      ^ call gas (asm [ DUP 6 ]) 0  0x0 0x0 0x0 0x0     -- call contract, swapping in address that was returned
      ^ asm [ POP, POP, STOP ] 
      -- ^ (loader $ add 3 4)                                 -- simple contract to add two numbers - offset is 30 
      -- ^ (loader $ add 3 4)                                 -- simple contract to add two numbers - offset is 30 
      ^ asm [ PUSH1 $ Literal len, DUP 1, PUSH1 $ Literal 0x0B, PUSH1 $ Literal 0, CODECOPY, PUSH1 $ Literal 0, RETURN ] -- this is the loader
      ^ asm [ PUSH1 $ Literal 2, PUSH1 $ Literal 3, ADD  ]    -- this is the contract code
      ^ Nil


  -- OK I think we need to be able to expose the labels.


  printLn code

  h <- fopen "out.vm" "w" 
  case h of 
    Right f => do
      printLn "whoot"
      fPutStr f code
      closeFile f
      pure ()


  pure ()

