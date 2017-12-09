{-
  https://queertypes.com/posts/48-logic-proofs-with-agda-coq-idris.html

Coq, equivalent,

Theorem and_distributes : forall a b c : Prop,
    (a /\ (b \/ c)) -> ((a /\ b) \/ (a /\ c)).
Proof.
  intros.
  destruct H as [HA [HB | HC]].
  Case "L". left. split. apply HA. apply HB.
  Case "R". right. split. apply HA.

-}

module Basics

%default total

data Or a b = Inl a | Inr b
data And a b = AndIntro a b

and_distributes_over_or : And a (Or b c) -> Or (And a b) (And a c)
and_distributes_over_or (AndIntro a x) =
 case x of
   Inl l => Inl (AndIntro a l)
   Inr r => Inr (AndIntro a r)


f : And a (Or b c) -> Or (And a b) (And a c)
f = id

