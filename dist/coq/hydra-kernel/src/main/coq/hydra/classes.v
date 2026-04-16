(* Type classes *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive TypeClass : Type :=
| TypeClass_Equality : forall (_ : unit) , TypeClass
| TypeClass_Ordering : forall (_ : unit) , TypeClass.

