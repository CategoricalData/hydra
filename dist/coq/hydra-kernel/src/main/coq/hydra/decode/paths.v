(* Term decoders for hydra.paths *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.errors hydra.paths hydra.lib.eithers hydra.lib.maps hydra.extract.core hydra.decode.core hydra.lib.maybes hydra.lib.strings.

Definition subtypeStep : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtypeStep) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let variantMap := (maps.fromList) ((cons) ((pair) ("annotatedBody"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_AnnotatedBody) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("applicationFunction"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_ApplicationFunction) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("applicationArgument"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_ApplicationArgument) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("eitherLeft"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_EitherLeft) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("eitherRight"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_EitherRight) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("forallBody"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_ForallBody) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("functionDomain"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_FunctionDomain) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("functionCodomain"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_FunctionCodomain) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("listElement"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_ListElement) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("mapKeys"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_MapKeys) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("mapValues"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_MapValues) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("maybeElement"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_MaybeElement) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("pairFirst"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_PairFirst) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("pairSecond"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_PairSecond) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("recordField"%string) (fun (input : Term) => ((eithers.map) (fun (t : Name) => (SubtypeStep_RecordField) (t))) (((hydra.decode.core.name) (cx)) (input)))) ((cons) ((pair) ("setElement"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_SetElement) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("unionField"%string) (fun (input : Term) => ((eithers.map) (fun (t : Name) => (SubtypeStep_UnionField) (t))) (((hydra.decode.core.name) (cx)) (input)))) ((cons) ((pair) ("wrappedType"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtypeStep_WrappedType) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil))))))))))))))))))) in let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (SubtypeStep)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtypePath : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtypePath) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) (SubtypeStep)) => b)) ((((decodeList) (subtypeStep)) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtypeNode : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtypeNode) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("label"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_label : string) => ((eithers.bind) (((((requireField) ("id"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_id : string) => (inr) ((Build_SubtypeNode) (field_name) (field_label) (field_id)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtypeEdge : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtypeEdge) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("source"%string)) (subtypeNode)) (fieldMap)) (cx))) (fun (field_source : SubtypeNode) => ((eithers.bind) (((((requireField) ("path"%string)) (subtypePath)) (fieldMap)) (cx))) (fun (field_path : SubtypePath) => ((eithers.bind) (((((requireField) ("target"%string)) (subtypeNode)) (fieldMap)) (cx))) (fun (field_target : SubtypeNode) => (inr) ((Build_SubtypeEdge) (field_source) (field_path) (field_target)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtypeGraph : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtypeGraph) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("nodes"%string)) ((decodeList) (subtypeNode))) (fieldMap)) (cx))) (fun (field_nodes : (list) (SubtypeNode)) => ((eithers.bind) (((((requireField) ("edges"%string)) ((decodeList) (subtypeEdge))) (fieldMap)) (cx))) (fun (field_edges : (list) (SubtypeEdge)) => (inr) ((Build_SubtypeGraph) (field_nodes) (field_edges))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtermStep : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtermStep) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let variantMap := (maps.fromList) ((cons) ((pair) ("annotatedBody"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_AnnotatedBody) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("applicationFunction"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_ApplicationFunction) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("applicationArgument"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_ApplicationArgument) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("lambdaBody"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_LambdaBody) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("unionCasesDefault"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_UnionCasesDefault) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("unionCasesBranch"%string) (fun (input : Term) => ((eithers.map) (fun (t : Name) => (SubtermStep_UnionCasesBranch) (t))) (((hydra.decode.core.name) (cx)) (input)))) ((cons) ((pair) ("letBody"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_LetBody) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("letBinding"%string) (fun (input : Term) => ((eithers.map) (fun (t : Name) => (SubtermStep_LetBinding) (t))) (((hydra.decode.core.name) (cx)) (input)))) ((cons) ((pair) ("listElement"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (SubtermStep_ListElement) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("mapKey"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (SubtermStep_MapKey) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("mapValue"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (SubtermStep_MapValue) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("maybeTerm"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_MaybeTerm) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("productTerm"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (SubtermStep_ProductTerm) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("recordField"%string) (fun (input : Term) => ((eithers.map) (fun (t : Name) => (SubtermStep_RecordField) (t))) (((hydra.decode.core.name) (cx)) (input)))) ((cons) ((pair) ("setElement"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (SubtermStep_SetElement) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("sumTerm"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_SumTerm) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("typeLambdaBody"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_TypeLambdaBody) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("typeApplicationTerm"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_TypeApplicationTerm) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("injectionTerm"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_InjectionTerm) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("wrappedTerm"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (SubtermStep_WrappedTerm) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil))))))))))))))))))))) in let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (SubtermStep)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtermPath : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtermPath) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) (SubtermStep)) => b)) ((((decodeList) (subtermStep)) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtermNode : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtermNode) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("label"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_label : string) => ((eithers.bind) (((((requireField) ("id"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_id : string) => (inr) ((Build_SubtermNode) (field_name) (field_label) (field_id)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtermEdge : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtermEdge) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("source"%string)) (subtermNode)) (fieldMap)) (cx))) (fun (field_source : SubtermNode) => ((eithers.bind) (((((requireField) ("path"%string)) (subtermPath)) (fieldMap)) (cx))) (fun (field_path : SubtermPath) => ((eithers.bind) (((((requireField) ("target"%string)) (subtermNode)) (fieldMap)) (cx))) (fun (field_target : SubtermNode) => (inr) ((Build_SubtermEdge) (field_source) (field_path) (field_target)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition subtermGraph : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SubtermGraph) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("nodes"%string)) ((decodeList) (subtermNode))) (fieldMap)) (cx))) (fun (field_nodes : (list) (SubtermNode)) => ((eithers.bind) (((((requireField) ("edges"%string)) ((decodeList) (subtermEdge))) (fieldMap)) (cx))) (fun (field_edges : (list) (SubtermEdge)) => (inr) ((Build_SubtermGraph) (field_nodes) (field_edges))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

