(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.pairs *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.

Axiom bimap : forall (a b c d : Type), (a -> c) -> (b -> d) -> (a * b) -> (c * d).
Arguments bimap {a b c d}.
Axiom first : forall (a b : Type), (a * b) -> a.
Arguments first {a b}.
Axiom second : forall (a b : Type), (a * b) -> b.
Arguments second {a b}.
