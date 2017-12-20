
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


  
  -- SO - how do we deal with this.... a construct for variable binding ....
  -- in lambda calculus - they are free variables.
  -- OK so this does in fact work...

  -- ok hang on. create can be treated as a function (with side-effects) that returns a value
  -- we should be able to rewrite the entire thing as an expression...

  -------------
  -- OK - free variables - have to be put on the environment. In order that we can 
  --  capture the environment in a closure. if we have to. we can optimise out later - if needed.

  -- read - just substitute var/debrujn index - with the value in the env
  -- write - not sure.

  -- so we need to model a stack/list of environment values  - in the evm.
  -- let x = y in y 3 . it's discussed in haskell book - and it gets compiled to debrujn indices...
  -- there's monadic chaining which is sequential. but that's different to treating it all as let bindings .
  
  -- lambda abstraction \x.123   where x is () ....
  
  -- variable binding of free functions. .
  -- actually the function is fixed.
  
  {- 
  -- VERY IMPORTANT - I think it might make sense to change to use the lambda representation and then a separate
      representing the op 
      actual operation. in the way that we had binop for + operations etc..
      need to find the example


    let binding of free variables is simply syntactic sugar.
      so do we really want to introduce something like this....
      do we parse the expression? - yes. so we can output debug.
        

    (λf.N) M

    Authors often introduce syntactic sugar, such as let, to permit writing the above in the more intuitive order

    let f = M in N 

    when it gets lambda'ised like this, we will need Apply etc.
    lambda f . var N, 

    OK let f = M in N . SO, this means we actually probably *do* want to change from
      the list notation. to a lambda expression... where the binding is expressed.

    look at how we represented it - for the idris lambda syntax.
  -}

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

  -- it would be easy to track stack usage and vars with linear code - but what about with jumps
  -- so instead approach from a lambda calc - and compile down?
  -- dup basically creates a limit to the number of vars that can be put on the stack.

  -- so we have pushing values onto the stack. but not returning values
  -- in any kind of meaningful way. or cleaning up stack.
  -- we have expression syntax that leaves a value. but then we have to dup it to use it somewhere
  -- and keep track of the stack... to know what to dup.
  -- each action - can be thought of as something that manipulates stack / mem... 

  -- just raw op-codes that get compiled isn't working. we need
  -- compilation is perhaps going to need to be a fold - propagating variables (or placeholders) 

  -- OR just assign vars. and patch up later? 


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

