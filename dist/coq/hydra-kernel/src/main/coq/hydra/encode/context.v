(* Term encoders for hydra.context *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.context hydra.core hydra.encode.core hydra.lib.lists hydra.lib.maps.

Definition context : forall (_ : Context_) , Term := fun (x : Context_) => (Term_Record) ((Build_Record_) ("hydra.context.Context"%string) ((cons) ((Build_Field) ("trace"%string) ((fun (xs : (list) (string)) => (Term_List) (((lists.map) (fun (x2 : string) => (Term_Literal) ((Literal_String) (x2)))) (xs))) ((fun r_ => (context__trace) (r_)) (x)))) ((cons) ((Build_Field) ("messages"%string) ((fun (xs : (list) (string)) => (Term_List) (((lists.map) (fun (x2 : string) => (Term_Literal) ((Literal_String) (x2)))) (xs))) ((fun r_ => (context__messages) (r_)) (x)))) ((cons) ((Build_Field) ("other"%string) ((fun (m : (list) ((prod) (Name) (Term))) => (Term_Map) ((((maps.bimap) (hydra.encode.core.name)) (hydra.encode.core.term)) (m))) ((fun r_ => (context__other) (r_)) (x)))) (nil))))).
Definition inContext (t0 : Type) : forall (_ : forall (_ : t0) , Term) , forall (_ : (InContext) (t0)) , Term := fun (e : forall (_ : t0) , Term) => fun (x : (InContext) (t0)) => (Term_Record) ((Build_Record_) ("hydra.context.InContext"%string) ((cons) ((Build_Field) ("object"%string) ((e) ((fun r_ => (inContext_object) (r_)) (x)))) ((cons) ((Build_Field) ("context"%string) ((context) ((fun r_ => (inContext_context) (r_)) (x)))) (nil)))).
Arguments inContext {t0}.

