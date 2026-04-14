(* Decoding functions for JSON data *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.json.model hydra.lib.maybes hydra.lib.eithers hydra.lib.maps hydra.lib.strings.

Definition decodeString : forall (_ : Value) , (sum) (string) (string) := fun x_ => match x_ with
| Value_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected a string"%string)
end.
Definition decodeOptionalField (t0 : Type) (t1 : Type) (t2 : Type) (t3 : Type) : forall (_ : forall (_ : t0) , (sum) (t1) (t2)) , forall (_ : t3) , forall (_ : (list) ((prod) (t3) (t0))) , (sum) (t1) ((option) (t2)) := fun (decodeValue : forall (_ : t0) , (sum) (t1) (t2)) => fun (name : t3) => fun (m : (list) ((prod) (t3) (t0))) => (((maybes.maybe) ((inr) (None))) (fun (v : t0) => ((eithers.map) (fun (x : t2) => (Some) (x))) ((decodeValue) (v)))) (((maps.lookup) (name)) (m)).
Arguments decodeOptionalField {t0} {t1} {t2} {t3}.
Definition decodeObject : forall (_ : Value) , (sum) (string) ((list) ((prod) (string) (Value))) := fun x_ => match x_ with
| Value_Object v_ => (fun (o : (list) ((prod) (string) (Value))) => (inr) (o)) (v_)
| _ => (inl) ("expected an object"%string)
end.
Definition decodeField (t0 : Type) (t1 : Type) : forall (_ : forall (_ : t0) , (sum) (string) (t1)) , forall (_ : string) , forall (_ : (list) ((prod) (string) (t0))) , (sum) (string) (t1) := fun (decodeValue : forall (_ : t0) , (sum) (string) (t1)) => fun (name : string) => fun (m : (list) ((prod) (string) (t0))) => ((eithers.bind) ((((decodeOptionalField) (decodeValue)) (name)) (m))) (((maybes.maybe) ((inl) (((strings.cat2) ("missing field: "%string)) (name)))) (fun (f : t1) => (inr) (f))).
Arguments decodeField {t0} {t1}.
Definition decodeBoolean : forall (_ : Value) , (sum) (string) (bool) := fun x_ => match x_ with
| Value_Boolean v_ => (fun (b : bool) => (inr) (b)) (v_)
| _ => (inl) ("expected a boolean"%string)
end.
Definition decodeArray (t0 : Type) : forall (_ : forall (_ : Value) , (sum) (string) (t0)) , forall (_ : Value) , (sum) (string) ((list) (t0)) := fun (decodeElem : forall (_ : Value) , (sum) (string) (t0)) => fun x_ => match x_ with
| Value_Array v_ => (fun (a : (list) (Value)) => ((eithers.mapList) (decodeElem)) (a)) (v_)
| _ => (inl) ("expected an array"%string)
end.
Arguments decodeArray {t0}.

