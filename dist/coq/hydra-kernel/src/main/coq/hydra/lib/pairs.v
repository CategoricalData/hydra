(* Hydra primitive library: hydra.lib.pairs *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.

Definition bimap (a b c d : Type) (f : a -> c) (g : b -> d) (p : a * b) : c * d :=
  (f (fst p), g (snd p)).
Arguments bimap {a b c d}.

Definition first (a b : Type) (p : a * b) : a := fst p.
Arguments first {a b}.

Definition second (a b : Type) (p : a * b) : b := snd p.
Arguments second {a b}.
