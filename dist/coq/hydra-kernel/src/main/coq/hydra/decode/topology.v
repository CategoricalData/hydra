(* Term decoders for hydra.topology *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.topology.

Definition vertex : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Z) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition graph_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((list) ((prod) (Z) ((list) (Z)))) := ((decodeMap) (vertex)) ((decodeList) (vertex)).
Definition tarjanState : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TarjanState) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("counter"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_counter : Z) => ((eithers.bind) (((((requireField) ("indices"%string)) (((decodeMap) (vertex)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_indices : (list) ((prod) (Z) (Z))) => ((eithers.bind) (((((requireField) ("lowLinks"%string)) (((decodeMap) (vertex)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_lowLinks : (list) ((prod) (Z) (Z))) => ((eithers.bind) (((((requireField) ("stack"%string)) ((decodeList) (vertex))) (fieldMap)) (cx))) (fun (field_stack : (list) (Z)) => ((eithers.bind) (((((requireField) ("onStack"%string)) ((decodeSet) (vertex))) (fieldMap)) (cx))) (fun (field_onStack : (list) (Z)) => ((eithers.bind) (((((requireField) ("sccs"%string)) ((decodeList) ((decodeList) (vertex)))) (fieldMap)) (cx))) (fun (field_sccs : (list) ((list) (Z))) => (inr) ((Build_TarjanState) (field_counter) (field_indices) (field_lowLinks) (field_stack) (field_onStack) (field_sccs))))))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

