(* Term encoders for hydra.classes *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.classes hydra.core.

Definition typeClass : forall (_ : TypeClass) , Term := fun x_ => match x_ with
| TypeClass_Equality v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.classes.TypeClass"%string) ((Build_Field) ("equality"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeClass_Ordering v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.classes.TypeClass"%string) ((Build_Field) ("ordering"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.

