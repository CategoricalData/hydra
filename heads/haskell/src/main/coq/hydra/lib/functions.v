(* Hydra primitive library: hydra.lib.functions *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import hydra.util.
Require Import hydra.lib.base.

Definition identity {x : Type} (v : x) : x := v.
Arguments identity {x}.
