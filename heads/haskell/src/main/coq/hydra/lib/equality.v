(* Hydra primitive library: hydra.core.lib.equality *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import hydra.core.util.
Require Import hydra.lib.base.

Definition equal {x : Type} (a b : x) : bool := hydra_eq a b.
Arguments equal {x}.
