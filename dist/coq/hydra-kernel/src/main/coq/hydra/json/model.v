(* A JSON syntax model. See the BNF at https://www.json.org *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive Value : Type :=
| Value_Array : forall (_ : (list) (Value)) , Value
| Value_Boolean : forall (_ : bool) , Value
| Value_Null : forall (_ : unit) , Value
| Value_Number : forall (_ : Q) , Value
| Value_Object : forall (_ : (list) ((prod) (string) (Value))) , Value
| Value_String : forall (_ : string) , Value.

