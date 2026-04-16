(* Hydra primitive library: hydra.lib.logic *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.

Definition and (a b : bool) : bool := andb a b.

Definition ifElse (x : Type) (b : bool) (t f : x) : x := if b then t else f.
Arguments ifElse {x}.

Definition not (a : bool) : bool := negb a.

Definition or (a b : bool) : bool := orb a b.
