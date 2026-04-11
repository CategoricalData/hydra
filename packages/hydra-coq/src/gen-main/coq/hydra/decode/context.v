(* Term decoders for hydra.context *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.errors hydra.context hydra.lib.eithers hydra.extract.core hydra.decode.core.

Definition context : hydra.graph.Graph -> Term -> (sum) (DecodingError) (Context_) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("trace"%string)) ((decodeList) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_trace : (list) (string)) => ((eithers.bind) (((((requireField) ("messages"%string)) ((decodeList) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_messages : (list) (string)) => ((eithers.bind) (((((requireField) ("other"%string)) (((decodeMap) (hydra.decode.core.name)) (hydra.decode.core.term))) (fieldMap)) (cx))) (fun (field_other : (list) ((prod) (Name) (Term))) => (inr) ((Build_Context_) (field_trace) (field_messages) (field_other)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition inContext (t0 : Type) : (hydra.graph.Graph -> Term -> (sum) (DecodingError) (t0)) -> hydra.graph.Graph -> Term -> (sum) (DecodingError) ((InContext) (t0)) :=
  fun (e : hydra.graph.Graph -> Term -> (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("object"%string)) (e)) (fieldMap)) (cx))) (fun (field_object : t0) => ((eithers.bind) (((((requireField) ("context"%string)) (context)) (fieldMap)) (cx))) (fun (field_context : Context_) => (inr) ((Build_InContext) (field_object) (field_context))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments inContext {t0}.

