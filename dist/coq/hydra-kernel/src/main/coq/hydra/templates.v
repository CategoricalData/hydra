(* A utility which instantiates a nonrecursive type with default values *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.errors hydra.lib.logic hydra.lib.eithers hydra.lib.maps hydra.lib.sets hydra.lib.maybes hydra.lib.strings hydra.graph hydra.decode.core.

Definition instantiateTemplate_bundle (t0 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : t0) , forall (_ : bool) , forall (_ : (list) ((prod) (Name) (Type_))) , forall (_ : Name) , forall (_ : Type_) , (sum) (Error) (Term)) =>
    let instantiateTemplate := bundle_ in
    fun (cx : t0) => fun (minimal : bool) => fun (schema : (list) ((prod) (Name) (Type_))) => fun (tname : Name) => fun (t : Type_) => let noPoly := fun (t1 : Type) => (inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("non-polymorphic type"%string) ("polymorphic or function type"%string)))) in let inst := fun (tn : Name) => ((((instantiateTemplate) (cx)) (minimal)) (schema)) (tn) in let forInteger := fun (it : IntegerType) => (fun x_ => match x_ with
| IntegerType_Bigint _ => (IntegerValue_Bigint) ((0)%Z)
| IntegerType_Int8 _ => (IntegerValue_Int8) ((0)%Z)
| IntegerType_Int16 _ => (IntegerValue_Int16) ((0)%Z)
| IntegerType_Int32 _ => (IntegerValue_Int32) ((0)%Z)
| IntegerType_Int64 _ => (IntegerValue_Int64) ((0)%Z)
| IntegerType_Uint8 _ => (IntegerValue_Uint8) ((0)%Z)
| IntegerType_Uint16 _ => (IntegerValue_Uint16) ((0)%Z)
| IntegerType_Uint32 _ => (IntegerValue_Uint32) ((0)%Z)
| IntegerType_Uint64 _ => (IntegerValue_Uint64) ((0)%Z)
end) (it) in let forFloat := fun (ft : FloatType) => (fun x_ => match x_ with
| FloatType_Bigfloat _ => (FloatValue_Bigfloat) ((0.0))
| FloatType_Float32 _ => (FloatValue_Float32) ((0.0))
| FloatType_Float64 _ => (FloatValue_Float64) ((0.0))
end) (ft) in let forLiteral := fun (lt : LiteralType) => (fun x_ => match x_ with
| LiteralType_Binary _ => (Literal_String) (""%string)
| LiteralType_Boolean _ => (Literal_Boolean) (false)
| LiteralType_Integer v_ => (fun (it : IntegerType) => (Literal_Integer) ((forInteger) (it))) (v_)
| LiteralType_Float v_ => (fun (ft : FloatType) => (Literal_Float) ((forFloat) (ft))) (v_)
| LiteralType_String _ => (Literal_String) (""%string)
end) (lt) in (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => ((inst) (tname)) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Application v_ => (fun (_ : ApplicationType) => (noPoly) (Term)) (v_)
| Type__Function v_ => (fun (_ : FunctionType) => (noPoly) (Term)) (v_)
| Type__Forall v_ => (fun (_ : ForallType) => (noPoly) (Term)) (v_)
| Type__List v_ => (fun (et : Type_) => (((logic.ifElse) (minimal)) ((inr) ((Term_List) (nil)))) (((eithers.bind) (((inst) (tname)) (et))) (fun (e : Term) => (inr) ((Term_List) ((cons) (e) (nil)))))) (v_)
| Type__Literal v_ => (fun (lt : LiteralType) => (inr) ((Term_Literal) ((forLiteral) (lt)))) (v_)
| Type__Map v_ => (fun (mt : MapType) => let vt := (fun r_ => (mapType_values) (r_)) (mt) in let kt := (fun r_ => (mapType_keys) (r_)) (mt) in (((logic.ifElse) (minimal)) ((inr) ((Term_Map) (maps.empty)))) (((eithers.bind) (((inst) (tname)) (kt))) (fun (ke : Term) => ((eithers.bind) (((inst) (tname)) (vt))) (fun (ve : Term) => (inr) ((Term_Map) (((maps.singleton) (ke)) (ve))))))) (v_)
| Type__Maybe v_ => (fun (ot : Type_) => (((logic.ifElse) (minimal)) ((inr) ((Term_Maybe) (None)))) (((eithers.bind) (((inst) (tname)) (ot))) (fun (e : Term) => (inr) ((Term_Maybe) ((Some) (e)))))) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => let toField := fun (ft : FieldType) => ((eithers.bind) (((inst) (tname)) ((fun r_ => (fieldType_type) (r_)) (ft)))) (fun (e : Term) => (inr) ((Build_Field) ((fun r_ => (fieldType_name) (r_)) (ft)) (e))) in ((eithers.bind) (((eithers.mapList) (toField)) (rt))) (fun (dfields : (list) (Field)) => (inr) ((Term_Record) ((Build_Record_) (tname) (dfields))))) (v_)
| Type__Set v_ => (fun (et : Type_) => (((logic.ifElse) (minimal)) ((inr) ((Term_Set) (sets.empty)))) (((eithers.bind) (((inst) (tname)) (et))) (fun (e : Term) => (inr) ((Term_Set) ((sets.fromList) ((cons) (e) (nil))))))) (v_)
| Type__Variable v_ => (fun (vname : Name) => (((maybes.maybe) ((inl) ((Error_Resolution) ((ResolutionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("bound type variable"%string) (((strings.cat2) ("unbound variable "%string)) ((fun w_ => w_) (vname)))))))) ((inst) (vname))) (((maps.lookup) (vname)) (schema))) (v_)
| Type__Wrap v_ => (fun (wt : Type_) => ((eithers.bind) (((inst) (tname)) (wt))) (fun (e : Term) => (inr) ((Term_Wrap) ((Build_WrappedTerm) (tname) (e))))) (v_)
| _ => hydra_unreachable
end) (t)).
Arguments instantiateTemplate_bundle {t0}.

Definition instantiateTemplate (t0 : Type) : forall (_ : t0) , forall (_ : bool) , forall (_ : (list) ((prod) (Name) (Type_))) , forall (_ : Name) , forall (_ : Type_) , (sum) (Error) (Term) :=
  instantiateTemplate_bundle.
Arguments instantiateTemplate {t0}.
Definition graphToSchema (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (Binding)) , (sum) (DecodingError) ((list) ((prod) (Name) (Type_))) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (els : (list) (Binding)) => let toPair := fun (el : Binding) => let name := (fun r_ => (binding_name) (r_)) (el) in ((eithers.bind) (((hydra.decode.core.type) (graph_)) ((fun r_ => (binding_term) (r_)) (el)))) (fun (t : Type_) => (inr) ((pair) (name) (t))) in ((eithers.bind) (((eithers.mapList) (toPair)) (els))) (fun (pairs : (list) ((prod) (Name) (Type_))) => (inr) ((maps.fromList) (pairs))).
Arguments graphToSchema {t0}.

