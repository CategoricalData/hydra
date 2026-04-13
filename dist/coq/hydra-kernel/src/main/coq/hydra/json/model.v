(* A JSON syntax model. See the BNF at https://www.json.org *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive Value : Type :=
| Value_Array : (list) (Value) -> Value
| Value_Boolean : bool -> Value
| Value_Null : unit -> Value
| Value_Number : Q -> Value
| Value_Object : (list) ((prod) (string) (Value)) -> Value
| Value_String : string -> Value.

