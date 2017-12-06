{-
  dsl
    http://docs.idris-lang.org/en/latest/tutorial/syntax.html

  for TTName
    https://www.idris-lang.org/docs/0.10/contrib_doc/docs/Language.Reflection.html#Language.Reflection.TTName
    pop stop - can be treated just like a natural number

  for index_first
    http://hackage.haskell.org/package/idris-1.1.1/docs/src/Idris-Parser-Data.html
    http://hackage.haskell.org/package/idris-0.9.12/docs/src/Idris-ParseData.html


  Ok this has let binding p7. also AP, and pure
    https://eb.host.cs.st-andrews.ac.uk/drafts/dsl-idris.pdf

  -- OK we have both let binding and lambdas, represented... which is rather amazing.
      would mean we could define embedded functions - and don't even need idris variables.

  -- OK the big question is - can we use do notation with this? ....
  -- I think possibly yes - because we're dealing with return values
  -- remembering that we're not using dsl for free vars. Instead we use idris.

  -- issue is that variables - are debruijn indexed - whereas we want to be able to 
      store stuff to memory / environment or stack.  
  -- but a debruijn index - may not be so bad - doesn't mean

  -- stuff like cons - might be able to be treated as effects however on the environment...

  do 
    x <- app (\lambda ) 123

  issue is that x is a free variable? not sure... 

  even if we have to use >>= it would probably be ok. not unlike ocaml

-}

%language DSLNotation 

data Expr : Type where

  -- Var takes pop and stop -- NO NOT SURE.
  Var : Expr -> Expr

  Val : Integer -> Expr
  Plus : Expr -> Expr -> Expr

  Stop : Expr
  Pop : Expr -> Expr

  Lambda : TTName -> Expr -> Expr

  Let :  TTName -> Expr -> Expr -> Expr

  -- App :  Expr -> Expr

dsl expr
  variable    = Var
  index_first = Stop
  index_next  = Pop
  -- lambda      = mkLam
  lambda      = Lambda
  -- what about application - probably just parse as (expr expr)
  -- there is support for let.
  -- but what about free variables - actually probably don't need. as we just use idris vars...

  -- will need an operator eg. app expr expr

  let = Let
  -- apply = App


human : Expr -> String 
human e = case e of 
  Var stack => "(var " ++ human stack ++ ")"
  Val val   => "(val " ++ show val ++ ")"
  Plus l r  => "(plus " ++ human l ++ " " ++ human r ++ ")"
  Stop      => "stop"
  Pop expr  => "(pop " ++ human expr ++ ")"
  Lambda (UN name) expr => "(lambda " ++ name ++ " " ++ human expr ++ ")"

  Let (UN name) expr1 expr2 => "(let " ++ name ++ " " ++ human expr1 ++ " " ++ human expr2  ++ ")"


-- Complete DSL expression - but how much of the types are exposed.
-- may not matter - we can have a correct by construction expression tree.

-- but we are not managing to prove anything about the computation 

-- this could construct a much more complicated type. representing expectations of args...
fact : Expr
-- fact = expr ( (\x => Plus x (Val 1))   )
-- fact = expr ( (\x,y => Plus x y )   )
fact = expr $ 
  let 
      -- f a b = Plus a b
      f = (\x,y => Plus x y)
      x = Val 123 
      y = Val 456
  in f 


main : IO ()
main = do
  printLn $ human Main.fact


-- don't think there's support for free variables... - but there is support for let binding?
-- what about apply? -- also using this syntax - we type the correct construction
-- of debruijn indices - but what about typing of lambda vars? 
-- No i think it's ok.... - the Pop and Stop will yield typed values

{-

mkLam : TTName -> Expr (t::g) t' -> Expr g (TyFun t t')
mkLam _ body = Lam body

fact : Expr G (TyFun TyInt TyInt)
fact = expr (\x => If (Op (==) x (Val 0))
                      (Val 1) (Op (*) (app fact (Op (-) x (Val 1))) x))
-}

