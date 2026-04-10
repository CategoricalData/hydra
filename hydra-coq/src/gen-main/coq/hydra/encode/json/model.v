(* Term encoders for hydra.json.model *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.json.model hydra.core hydra.lib.lists hydra.lib.maps.

Definition value_bundle :=
  hydra_fix (fun (bundle_ : Value -> Term) =>
    let value := bundle_ in
    fun x_ => match x_ with
| Value_Array v_ => (fun (y : (list) (Value)) => (Term_Union) ((Build_Injection) ("Value"%string) ((Build_Field) ("array"%string) ((fun (xs : (list) (Value)) => (Term_List) (((lists.map) (value)) (xs))) (y))))) (v_)
| Value_Boolean v_ => (fun (y : bool) => (Term_Union) ((Build_Injection) ("Value"%string) ((Build_Field) ("boolean"%string) ((fun (x : bool) => (Term_Literal) ((Literal_Boolean) (x))) (y))))) (v_)
| Value_Null v_ => (fun (y : unit) => (Term_Union) ((Build_Injection) ("Value"%string) ((Build_Field) ("null"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Value_Number v_ => (fun (y : Q) => (Term_Union) ((Build_Injection) ("Value"%string) ((Build_Field) ("number"%string) ((fun (x : Q) => (Term_Literal) ((Literal_Float) ((FloatValue_Bigfloat) (x)))) (y))))) (v_)
| Value_Object v_ => (fun (y : (list) ((prod) (string) (Value))) => (Term_Union) ((Build_Injection) ("Value"%string) ((Build_Field) ("object"%string) ((fun (m : (list) ((prod) (string) (Value))) => (Term_Map) ((((maps.bimap) (fun (x : string) => (Term_Literal) ((Literal_String) (x)))) (value)) (m))) (y))))) (v_)
| Value_String v_ => (fun (y : string) => (Term_Union) ((Build_Injection) ("Value"%string) ((Build_Field) ("string"%string) ((fun (x : string) => (Term_Literal) ((Literal_String) (x))) (y))))) (v_)
end).

Definition value : Value -> Term :=
  value_bundle.

