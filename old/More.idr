

{-

 776         0xf0 -> do
 777           case stk of
 778             (xValue:xOffset:xSize:xs) ->
 779               burn g_create $ do


 827         -- op: CALL
 828         0xf1 ->
 829           case stk of
 830             ( xGas
 831               : (num -> xTo)
 832               : xValue
 833               : xInOffset
 834               : xInSize
 835               : xOutOffset
 836               : xOutSize
 837               : xs
 838              ) -> do
-}



-- Ahhhh will might struggle to push a large integer?
-- OK. manipulating this thing as a string of bytes is going to be royal pain...

myerror : Void
myerror = error "whoot"

main' : IO ()
main' = do

  -- call(g, a, v, in, insize, out, outsize)
  -- let ops = compile $ call gas 0xaebc05cb911a4ec6f541c3590deebab8fca797fb 0x0 0x0 0x0 0x0 0x0

  -- ok. problem. is that the memory is not an expressoin input.

  -- it might be that we need a single argument for the call method...
  -- but we should still be able to get into that space

  -- THIS IS NOT CALLDATA!!!!!
  -- NOT TO BE USED WITH hevm or with ethrun

  -- There is thus also a new RETURN opcode which allows contract execution to return data.
  -- So what is used? the fucking outdata or return or the stack ?

  {-
    https://ethereum.stackexchange.com/questions/8044/what-are-the-two-arguments-to-a-return-opcode

    The 2 arguments to the RETURN opcode are offsets into memory: the starting and ending offset.
    The EVM execution is stopped and data consisting of the memory bytes from [start, end-1] are the output of the execution.
    Example:

    If memory is [5, 6, 7, 8, 9, 10], a return with offsets 1, 4 would produce a result (output) of 3 bytes (6, 7, 8).
  -}

  -- so what do we want to do here...
  -- lets check the sub works...

  printLn "before error"
  let x = the Void $ error "whoot"
  let x' = the Integer $ idris_crash "whoot"
  printLn "after error"

  -- 40 should be the a
  let ops =
--       (compile $ mstore 0x60 0x40 )  -- eg. 60 into 40
--    ++ (compile $ mload 0x40 )        -- test load at 40
--    [] ++ (compile $ call gas address 0 0x0 0x0 0x0 32)
      (compile $ minus 4 3 )

-- what is the relationship...
-- calldataload

  let hops = map human ops
  printLn hops

  let mops = foldl (++) "" $ map machine ops
  printLn mops





