(* Term decoders for hydra.phantoms *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.errors hydra.phantoms hydra.lib.eithers hydra.decode.core hydra.extract.core.

Definition tTerm (t0 : Type) (t1 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((TTerm) (t1)) := fun (a : t0) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : Term) => b)) (((hydra.decode.core.term) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments tTerm {t0} {t1}.
Definition tTermDefinition (t0 : Type) (t1 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((TTermDefinition) (t1)) := fun (a : t0) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("term"%string)) ((tTerm) (a))) (fieldMap)) (cx))) (fun (field_term : (TTerm) (t1)) => (inr) ((Build_TTermDefinition) (field_name) (field_term))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments tTermDefinition {t0} {t1}.
Definition tBinding (t0 : Type) (t1 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((TBinding) (t1)) := fun (a : t0) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("term"%string)) ((tTerm) (a))) (fieldMap)) (cx))) (fun (field_term : (TTerm) (t1)) => (inr) ((Build_TBinding) (field_name) (field_term))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments tBinding {t0} {t1}.

