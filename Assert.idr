
This doesn't work - because system exit won't exit in the interpreter

-- myexit: a -> a
--  unsafePerformIO ( exit 123)


myexit : Int -> IO a
myexit code = believe_me $ foreign FFI_C "exit" (Int -> IO ()) code

mytrace : (msg : String) -> (result : a) -> a
mytrace x val = unsafePerformIO {ffi=FFI_C} (
  --do putStrLn x; 
      myexit 123
      --pure val
)



main

  printLn "before"
  let j = mytrace "whoot no good" 123 
  printLn "after"


-
