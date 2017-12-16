
import Env


m : Item 
m = C 123 :: (L $ NIL :: C 123 ) :: C 789


main : IO ()
main = do

  printLn m 

