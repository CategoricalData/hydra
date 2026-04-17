(* Term decoders for hydra.typing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.context hydra.core hydra.decode.context hydra.decode.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.typing.

Definition functionStructure (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((FunctionStructure) (t0)) := fun (env : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("typeParams"%string)) ((decodeList) (hydra.decode.core.name))) (fieldMap)) (cx))) (fun (field_typeParams : (list) (Name)) => ((eithers.bind) (((((requireField) ("params"%string)) ((decodeList) (hydra.decode.core.name))) (fieldMap)) (cx))) (fun (field_params : (list) (Name)) => ((eithers.bind) (((((requireField) ("bindings"%string)) ((decodeList) (hydra.decode.core.binding))) (fieldMap)) (cx))) (fun (field_bindings : (list) (Binding)) => ((eithers.bind) (((((requireField) ("body"%string)) (hydra.decode.core.term)) (fieldMap)) (cx))) (fun (field_body : Term) => ((eithers.bind) (((((requireField) ("domains"%string)) ((decodeList) (hydra.decode.core.type))) (fieldMap)) (cx))) (fun (field_domains : (list) (Type_)) => ((eithers.bind) (((((requireField) ("codomain"%string)) ((decodeMaybe) (hydra.decode.core.type))) (fieldMap)) (cx))) (fun (field_codomain : (option) (Type_)) => ((eithers.bind) (((((requireField) ("environment"%string)) (env)) (fieldMap)) (cx))) (fun (field_environment : t0) => (inr) ((Build_FunctionStructure) (field_typeParams) (field_params) (field_bindings) (field_body) (field_domains) (field_codomain) (field_environment)))))))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments functionStructure {t0}.
Definition typeSubst : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeSubst) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TypeSubst))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) ((prod) (Name) (Type_))) => b)) (((((decodeMap) (hydra.decode.core.name)) (hydra.decode.core.type)) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (TypeSubst)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition inferenceResult : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (InferenceResult) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (InferenceResult))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("term"%string)) (hydra.decode.core.term)) (fieldMap)) (cx))) (fun (field_term : Term) => ((eithers.bind) (((((requireField) ("type"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_type : Type_) => ((eithers.bind) (((((requireField) ("subst"%string)) (typeSubst)) (fieldMap)) (cx))) (fun (field_subst : TypeSubst) => ((eithers.bind) (((((requireField) ("classConstraints"%string)) (((decodeMap) (hydra.decode.core.name)) (hydra.decode.core.typeVariableMetadata))) (fieldMap)) (cx))) (fun (field_classConstraints : (list) ((prod) (Name) (TypeVariableMetadata))) => ((eithers.bind) (((((requireField) ("context"%string)) (hydra.decode.context.context)) (fieldMap)) (cx))) (fun (field_context : Context_) => ((inr) ((Build_InferenceResult) (field_term) (field_type) (field_subst) (field_classConstraints) (field_context))) : (sum) (DecodingError) (InferenceResult))))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (InferenceResult)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition termSubst : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TermSubst) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TermSubst))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) ((prod) (Name) (Term))) => b)) (((((decodeMap) (hydra.decode.core.name)) (hydra.decode.core.term)) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (TermSubst)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition typeConstraint : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeConstraint) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TypeConstraint))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("left"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_left : Type_) => ((eithers.bind) (((((requireField) ("right"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_right : Type_) => ((eithers.bind) (((((requireField) ("comment"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_comment : string) => ((inr) ((Build_TypeConstraint) (field_left) (field_right) (field_comment))) : (sum) (DecodingError) (TypeConstraint))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (TypeConstraint)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

