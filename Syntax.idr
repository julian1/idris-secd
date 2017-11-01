
ifThenElse : (x:Bool) -> Lazy a -> Lazy a -> a;
ifThenElse True  t e = t;
ifThenElse False t e = e;

-- and then extend the core syntax with a syntax declaration:

syntax "if" [test] "then" [t] "else" [e] = ifThenElse test t e;


