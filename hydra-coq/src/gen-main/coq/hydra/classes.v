(* Type classes *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive TypeClass : Type :=
| TypeClass_Equality : unit -> TypeClass
| TypeClass_Ordering : unit -> TypeClass.

