(* Utilities for extracting values from JSON objects *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.json.model hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings.

Definition showValue (t0 : Type) : forall (_ : t0) , string := fun (value : t0) => "TODO: implement showValue"%string.
Arguments showValue {t0}.
Definition expectArray : forall (_ : Value) , (sum) (string) ((list) (Value)) := fun (value : Value) => (fun x_ => match x_ with
| Value_Array v_ => (fun (els : (list) (Value)) => ((inr) (els)) : (sum) (string) ((list) (Value))) (v_)
| _ => ((inl) (((strings.cat2) (((strings.cat2) ("expected "%string)) ("JSON array"%string))) (((strings.cat2) (" but found "%string)) ((showValue) (value))))) : (sum) (string) ((list) (Value))
end) (value).
Definition expectNumber : forall (_ : Value) , (sum) (string) (Q) := fun (value : Value) => (fun x_ => match x_ with
| Value_Number v_ => (fun (d : Q) => ((inr) (d)) : (sum) (string) (Q)) (v_)
| _ => ((inl) (((strings.cat2) (((strings.cat2) ("expected "%string)) ("JSON number"%string))) (((strings.cat2) (" but found "%string)) ((showValue) (value))))) : (sum) (string) (Q)
end) (value).
Definition expectObject : forall (_ : Value) , (sum) (string) ((list) ((prod) (string) (Value))) := fun (value : Value) => (fun x_ => match x_ with
| Value_Object v_ => (fun (m : (list) ((prod) (string) (Value))) => ((inr) (m)) : (sum) (string) ((list) ((prod) (string) (Value)))) (v_)
| _ => ((inl) (((strings.cat2) (((strings.cat2) ("expected "%string)) ("JSON object"%string))) (((strings.cat2) (" but found "%string)) ((showValue) (value))))) : (sum) (string) ((list) ((prod) (string) (Value)))
end) (value).
Definition expectString : forall (_ : Value) , (sum) (string) (string) := fun (value : Value) => (fun x_ => match x_ with
| Value_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (string) (string)) (v_)
| _ => ((inl) (((strings.cat2) (((strings.cat2) ("expected "%string)) ("JSON string"%string))) (((strings.cat2) (" but found "%string)) ((showValue) (value))))) : (sum) (string) (string)
end) (value).
Definition opt (t0 : Type) (t1 : Type) : forall (_ : t0) , forall (_ : (list) ((prod) (t0) (t1))) , (option) (t1) := fun (fname : t0) => fun (m : (list) ((prod) (t0) (t1))) => ((maps.lookup) (fname)) (m).
Arguments opt {t0} {t1}.
Definition optArray (t0 : Type) : forall (_ : t0) , forall (_ : (list) ((prod) (t0) (Value))) , (sum) (string) ((option) ((list) (Value))) := fun (fname : t0) => fun (m : (list) ((prod) (t0) (Value))) => (((maybes.maybe) ((inr) (None))) (fun (a : Value) => ((eithers.map) (fun (x : (list) (Value)) => (Some) (x))) ((expectArray) (a)))) (((opt) (fname)) (m)).
Arguments optArray {t0}.
Definition optString (t0 : Type) : forall (_ : t0) , forall (_ : (list) ((prod) (t0) (Value))) , (sum) (string) ((option) (string)) := fun (fname : t0) => fun (m : (list) ((prod) (t0) (Value))) => (((maybes.maybe) ((inr) (None))) (fun (s : Value) => ((eithers.map) (fun (x : string) => (Some) (x))) ((expectString) (s)))) (((opt) (fname)) (m)).
Arguments optString {t0}.
Definition require (t0 : Type) (t1 : Type) : forall (_ : t0) , forall (_ : (list) ((prod) (t0) (t1))) , (sum) (string) (t1) := fun (fname : t0) => fun (m : (list) ((prod) (t0) (t1))) => (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("required attribute "%string) ((cons) ((showValue) (fname)) ((cons) (" not found"%string) (nil))))))) (fun (value : t1) => (inr) (value))) (((maps.lookup) (fname)) (m)).
Arguments require {t0} {t1}.
Definition requireArray (t0 : Type) : forall (_ : t0) , forall (_ : (list) ((prod) (t0) (Value))) , (sum) (string) ((list) (Value)) := fun (fname : t0) => fun (m : (list) ((prod) (t0) (Value))) => ((eithers.bind) (((require) (fname)) (m))) (expectArray).
Arguments requireArray {t0}.
Definition requireNumber (t0 : Type) : forall (_ : t0) , forall (_ : (list) ((prod) (t0) (Value))) , (sum) (string) (Q) := fun (fname : t0) => fun (m : (list) ((prod) (t0) (Value))) => ((eithers.bind) (((require) (fname)) (m))) (expectNumber).
Arguments requireNumber {t0}.
Definition requireString (t0 : Type) : forall (_ : t0) , forall (_ : (list) ((prod) (t0) (Value))) , (sum) (string) (string) := fun (fname : t0) => fun (m : (list) ((prod) (t0) (Value))) => ((eithers.bind) (((require) (fname)) (m))) (expectString).
Arguments requireString {t0}.

