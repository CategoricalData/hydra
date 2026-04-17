(* Extraction and validation for hydra.core types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.errors hydra.graph hydra.lexical hydra.lib.eithers hydra.lib.equality hydra.lib.lists hydra.lib.literals hydra.lib.logic hydra.lib.maps hydra.lib.maybes hydra.lib.pairs hydra.lib.sets hydra.lib.strings hydra.show.core hydra.show.errors hydra.strip.

Definition bigfloatValue : forall (_ : FloatValue) , (sum) (Error) (Q) := fun (v : FloatValue) => (fun x_ => match x_ with
| FloatValue_Bigfloat v_ => (fun (f : Q) => ((inr) (f)) : (sum) (Error) (Q)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("bigfloat"%string) ((float) (v)))))) : (sum) (Error) (Q)
end) (v).
Definition literal : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Literal) := fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (lit : Literal) => ((inr) (lit)) : (sum) (Error) (Literal)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("literal"%string) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (Literal)
end) (term_)).
Definition floatLiteral : forall (_ : Literal) , (sum) (Error) (FloatValue) := fun (lit : Literal) => (fun x_ => match x_ with
| Literal_Float v_ => (fun (v : FloatValue) => ((inr) (v)) : (sum) (Error) (FloatValue)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("floating-point value"%string) ((hydra.show.core.literal) (lit)))))) : (sum) (Error) (FloatValue)
end) (lit).
Definition bigfloat : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Q) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((floatLiteral) (l))) (fun (f : FloatValue) => (bigfloatValue) (f))).
Definition bigintValue : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Bigint v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("bigint"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition integerLiteral : forall (_ : Literal) , (sum) (Error) (IntegerValue) := fun (lit : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun (v : IntegerValue) => ((inr) (v)) : (sum) (Error) (IntegerValue)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("integer value"%string) ((hydra.show.core.literal) (lit)))))) : (sum) (Error) (IntegerValue)
end) (lit).
Definition bigint : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (bigintValue) (i))).
Definition binaryLiteral : forall (_ : Literal) , (sum) (Error) (string) := fun (v : Literal) => (fun x_ => match x_ with
| Literal_Binary v_ => (fun (b : string) => ((inr) (b)) : (sum) (Error) (string)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("binary"%string) ((hydra.show.core.literal) (v)))))) : (sum) (Error) (string)
end) (v).
Definition binary : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (string) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => (binaryLiteral) (l)).
Definition booleanLiteral : forall (_ : Literal) , (sum) (Error) (bool) := fun (v : Literal) => (fun x_ => match x_ with
| Literal_Boolean v_ => (fun (b : bool) => ((inr) (b)) : (sum) (Error) (bool)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("boolean"%string) ((hydra.show.core.literal) (v)))))) : (sum) (Error) (bool)
end) (v).
Definition boolean : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (bool) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => (booleanLiteral) (l)).
Definition cases : forall (_ : Name) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (CaseStatement) := fun (name : Name) => fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Cases v_ => (fun (cs : CaseStatement) => (((logic.ifElse) (((equality.equal) ((fun w_ => w_) ((fun r_ => (caseStatement_typeName) (r_)) (cs)))) ((fun w_ => w_) (name)))) (((inr) (cs)) : (sum) (Error) (CaseStatement))) (((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) ("case statement for type "%string)) ((fun w_ => w_) (name))) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (CaseStatement))) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("case statement"%string) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (CaseStatement)
end) (term_)).
Definition caseField : forall (_ : Name) , forall (_ : string) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Field) := fun (name : Name) => fun (n : string) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => let fieldName := n in ((eithers.bind) ((((cases) (name)) (graph_)) (term_))) (fun (cs : CaseStatement) => let matching := ((lists.filter) (fun (f : Field) => ((equality.equal) ((fun w_ => w_) ((fun r_ => (field_name) (r_)) (f)))) ((fun w_ => w_) (fieldName)))) ((fun r_ => (caseStatement_cases) (r_)) (cs)) in (((logic.ifElse) ((lists.null) (matching))) (((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("matching case"%string) ("no matching case"%string))))) : (sum) (Error) (Field))) (((inr) ((lists.head) (matching))) : (sum) (Error) (Field))).
Definition decimalLiteral : forall (_ : Literal) , (sum) (Error) (Q) := fun (v : Literal) => (fun x_ => match x_ with
| Literal_Decimal v_ => (fun (d : Q) => ((inr) (d)) : (sum) (Error) (Q)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("decimal"%string) ((hydra.show.core.literal) (v)))))) : (sum) (Error) (Q)
end) (v).
Definition decimal : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Q) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => (decimalLiteral) (l)).
Definition pair_ (t0 : Type) (t1 : Type) : forall (_ : forall (_ : Term) , (sum) (Error) (t0)) , forall (_ : forall (_ : Term) , (sum) (Error) (t1)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((prod) (t0) (t1)) := fun (kf : forall (_ : Term) , (sum) (Error) (t0)) => fun (vf : forall (_ : Term) , (sum) (Error) (t1)) => fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Pair v_ => (fun (p : (prod) (Term) (Term)) => ((eithers.bind) ((kf) ((pairs.first) (p)))) (fun (kVal : t0) => ((eithers.bind) ((vf) ((pairs.second) (p)))) (fun (vVal : t1) => (inr) ((pair) (kVal) (vVal))))) (v_)
| _ => (inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("pair"%string) ((hydra.show.core.term) (term_)))))
end) (term_)).
Arguments pair_ {t0} {t1}.
Definition map_bundle (t0 : Type) (t1 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : forall (_ : Term) , (sum) (Error) (t0)) , forall (_ : forall (_ : Term) , (sum) (Error) (t1)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) ((prod) (t0) (t1)))) =>
    let map := bundle_ in
    fun (fk : forall (_ : Term) , (sum) (Error) (t0)) => fun (fv : forall (_ : Term) , (sum) (Error) (t1)) => fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => let pair_ := fun (kvPair : (prod) (Term) (Term)) => let kterm := (pairs.first) (kvPair) in let vterm := (pairs.second) (kvPair) in ((eithers.bind) ((fk) (kterm))) (fun (kval : t0) => ((eithers.bind) ((fv) (vterm))) (fun (vval : t1) => (inr) ((pair) (kval) (vval)))) in ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Map v_ => (fun (m : (list) ((prod) (Term) (Term))) => ((eithers.map) (maps.fromList)) (((eithers.mapList) (pair_)) ((maps.toList) (m)))) (v_)
| _ => (inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("map"%string) ((hydra.show.core.term) (term_)))))
end) (term_))).
Arguments map_bundle {t0} {t1}.

Definition map (t0 : Type) (t1 : Type) : forall (_ : forall (_ : Term) , (sum) (Error) (t0)) , forall (_ : forall (_ : Term) , (sum) (Error) (t1)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) ((prod) (t0) (t1))) :=
  map_bundle.
Arguments map {t0} {t1}.
Definition stripWithDecodingError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Term) := fun (g : hydra.graph.Graph) => fun (term_ : Term) => (((eithers.bimap) (fun (_e : Error) => (hydra.show.errors.error) (_e))) (fun (x : Term) => x)) (((stripAndDereferenceTermEither) (g)) (term_)).
Definition decodeEither (t0 : Type) (t1 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t1)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((sum) (t0) (t1)) := fun (leftDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (rightDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t1)) => fun (g : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripWithDecodingError) (g)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Either v_ => (fun (e : (sum) (Term) (Term)) => (((eithers.either) (fun (lv : Term) => ((eithers.map) (fun (x : t0) => (inl) (x))) (((leftDecoder) (g)) (lv)))) (fun (rv : Term) => ((eithers.map) (fun (x : t1) => (inr) (x))) (((rightDecoder) (g)) (rv)))) (e)) (v_)
| _ => (inl) ("expected either value"%string)
end) (stripped)).
Arguments decodeEither {t0} {t1}.
Definition decodeList (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((list) (t0)) := fun (elemDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (g : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripWithDecodingError) (g)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_List v_ => (fun (els : (list) (Term)) => ((eithers.mapList) ((elemDecoder) (g))) (els)) (v_)
| _ => (inl) ("expected list"%string)
end) (stripped)).
Arguments decodeList {t0}.
Definition decodeMap (t0 : Type) (t1 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t1)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((list) ((prod) (t0) (t1))) := fun (keyDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (valDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t1)) => fun (g : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripWithDecodingError) (g)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Map v_ => (fun (m : (list) ((prod) (Term) (Term))) => ((eithers.map) (maps.fromList)) (((eithers.mapList) (fun (kv : (prod) (Term) (Term)) => ((eithers.bind) (((keyDecoder) (g)) ((pairs.first) (kv)))) (fun (k : t0) => ((eithers.map) (fun (v : t1) => (pair) (k) (v))) (((valDecoder) (g)) ((pairs.second) (kv)))))) ((maps.toList) (m)))) (v_)
| _ => (inl) ("expected map"%string)
end) (stripped)).
Arguments decodeMap {t0} {t1}.
Definition decodeMaybe (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((option) (t0)) := fun (elemDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (g : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripWithDecodingError) (g)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Maybe v_ => (fun (opt : (option) (Term)) => ((eithers.mapMaybe) ((elemDecoder) (g))) (opt)) (v_)
| _ => (inl) ("expected optional value"%string)
end) (stripped)).
Arguments decodeMaybe {t0}.
Definition decodePair (t0 : Type) (t1 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t1)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((prod) (t0) (t1)) := fun (firstDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (secondDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t1)) => fun (g : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripWithDecodingError) (g)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Pair v_ => (fun (p : (prod) (Term) (Term)) => ((eithers.bind) (((firstDecoder) (g)) ((pairs.first) (p)))) (fun (f : t0) => ((eithers.map) (fun (s : t1) => (pair) (f) (s))) (((secondDecoder) (g)) ((pairs.second) (p))))) (v_)
| _ => (inl) ("expected pair"%string)
end) (stripped)).
Arguments decodePair {t0} {t1}.
Definition decodeSet (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((list) (t0)) := fun (elemDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (g : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripWithDecodingError) (g)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Set v_ => (fun (s : (list) (Term)) => ((eithers.map) (sets.fromList)) (((eithers.mapList) ((elemDecoder) (g))) ((sets.toList) (s)))) (v_)
| _ => (inl) ("expected set"%string)
end) (stripped)).
Arguments decodeSet {t0}.
Definition decodeUnit : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (unit) := fun (g : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripWithDecodingError) (g)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Unit _ => ((inr) (tt)) : (sum) (DecodingError) (unit)
| _ => ((inl) ("expected a unit value"%string)) : (sum) (DecodingError) (unit)
end) (stripped)).
Definition decodeWrapped (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0) := fun (bodyDecoder : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (g : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripWithDecodingError) (g)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wt : WrappedTerm) => ((bodyDecoder) (g)) ((fun r_ => (wrappedTerm_body) (r_)) (wt))) (v_)
| _ => (inl) ("expected wrapped value"%string)
end) (stripped)).
Arguments decodeWrapped {t0}.
Definition eitherTerm (t0 : Type) (t1 : Type) : forall (_ : forall (_ : Term) , (sum) (Error) (t0)) , forall (_ : forall (_ : Term) , (sum) (Error) (t1)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((sum) (t0) (t1)) := fun (leftFun : forall (_ : Term) , (sum) (Error) (t0)) => fun (rightFun : forall (_ : Term) , (sum) (Error) (t1)) => fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Either v_ => (fun (et : (sum) (Term) (Term)) => (((eithers.either) (fun (l : Term) => ((eithers.map) (fun (x : t0) => (inl) (x))) ((leftFun) (l)))) (fun (r : Term) => ((eithers.map) (fun (x : t1) => (inr) (x))) ((rightFun) (r)))) (et)) (v_)
| _ => (inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("either value"%string) ((hydra.show.core.term) (term_)))))
end) (term_)).
Arguments eitherTerm {t0} {t1}.
Definition eitherType : forall (_ : Type_) , (sum) (Error) (EitherType) := fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Either v_ => (fun (et : EitherType) => ((inr) (et)) : (sum) (Error) (EitherType)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("either type"%string) ((hydra.show.core.type) (typ)))))) : (sum) (Error) (EitherType)
end) (stripped).
Definition field (t0 : Type) : forall (_ : Name) , forall (_ : forall (_ : Term) , (sum) (Error) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (Field)) , (sum) (Error) (t0) := fun (fname : Name) => fun (mapping : forall (_ : Term) , (sum) (Error) (t0)) => fun (graph_ : hydra.graph.Graph) => fun (fields : (list) (Field)) => let matchingFields := ((lists.filter) (fun (f : Field) => ((equality.equal) ((fun w_ => w_) ((fun r_ => (field_name) (r_)) (f)))) ((fun w_ => w_) (fname)))) (fields) in (((logic.ifElse) ((lists.null) (matchingFields))) ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) ("field "%string)) ((fun w_ => w_) (fname))) ("no matching field"%string)))))) ((((logic.ifElse) (((equality.equal) ((lists.length) (matchingFields))) ((1)%Z))) (((eithers.bind) (((stripAndDereferenceTerm) (graph_)) ((fun r_ => (field_term) (r_)) ((lists.head) (matchingFields))))) (fun (stripped : Term) => (mapping) (stripped)))) ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("single field"%string) (((strings.cat2) ("multiple fields named "%string)) ((fun w_ => w_) (fname)))))))).
Arguments field {t0}.
Definition float32Value : forall (_ : FloatValue) , (sum) (Error) (Q) := fun (v : FloatValue) => (fun x_ => match x_ with
| FloatValue_Float32 v_ => (fun (f : Q) => ((inr) (f)) : (sum) (Error) (Q)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("float32"%string) ((float) (v)))))) : (sum) (Error) (Q)
end) (v).
Definition float32 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Q) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((floatLiteral) (l))) (fun (f : FloatValue) => (float32Value) (f))).
Definition float64Value : forall (_ : FloatValue) , (sum) (Error) (Q) := fun (v : FloatValue) => (fun x_ => match x_ with
| FloatValue_Float64 v_ => (fun (f : Q) => ((inr) (f)) : (sum) (Error) (Q)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("float64"%string) ((float) (v)))))) : (sum) (Error) (Q)
end) (v).
Definition float64 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Q) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((floatLiteral) (l))) (fun (f : FloatValue) => (float64Value) (f))).
Definition floatValue : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (FloatValue) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => (floatLiteral) (l)).
Definition functionType : forall (_ : Type_) , (sum) (Error) (FunctionType) := fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Function v_ => (fun (ft : FunctionType) => ((inr) (ft)) : (sum) (Error) (FunctionType)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("function type"%string) ((hydra.show.core.type) (typ)))))) : (sum) (Error) (FunctionType)
end) (stripped).
Definition injection_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Name) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Field)) =>
    let injection := bundle_ in
    fun (expected : Name) => fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (injection : Injection) => (((logic.ifElse) (((equality.equal) ((fun w_ => w_) ((fun r_ => (injection_typeName) (r_)) (injection)))) ((fun w_ => w_) (expected)))) (((inr) ((fun r_ => (injection_field) (r_)) (injection))) : (sum) (Error) (Field))) (((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) ("injection of type "%string)) ((fun w_ => w_) (expected))) ((fun w_ => w_) ((fun r_ => (injection_typeName) (r_)) (injection))))))) : (sum) (Error) (Field))) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("injection"%string) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (Field)
end) (term_))).

Definition injection : forall (_ : Name) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Field) :=
  injection_bundle.
Definition int16Value : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Int16 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("int16"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition int16 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (int16Value) (i))).
Definition int32Value : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("int32"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition int32 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (int32Value) (i))).
Definition int64Value : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Int64 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("int64"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition int64 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (int64Value) (i))).
Definition int8Value : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Int8 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("int8"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition int8 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (int8Value) (i))).
Definition integerValue : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (IntegerValue) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => (integerLiteral) (l)).
Definition lambda : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Lambda) := fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Lambda v_ => (fun (l : Lambda) => ((inr) (l)) : (sum) (Error) (Lambda)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("lambda"%string) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (Lambda)
end) (term_)).
Definition lambdaBody : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term) := fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.map) (fun r_ => (lambda_body) (r_))) (((lambda) (graph_)) (term_)).
Definition let_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Let) := fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Let v_ => (fun (lt : Let) => ((inr) (lt)) : (sum) (Error) (Let)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("let term"%string) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (Let)
end) (term_)).
Definition letBinding : forall (_ : string) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term) := fun (n : string) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => let name := n in ((eithers.bind) (((let_) (graph_)) (term_))) (fun (letExpr : Let) => let matchingBindings := ((lists.filter) (fun (b : Binding) => ((equality.equal) ((fun w_ => w_) ((fun r_ => (binding_name) (r_)) (b)))) ((fun w_ => w_) (name)))) ((fun r_ => (let_bindings) (r_)) (letExpr)) in (((logic.ifElse) ((lists.null) (matchingBindings))) (((inl) ((Error_Extraction) ((ExtractionError_NoSuchBinding) ((Build_NoSuchBindingError) (name))))) : (sum) (Error) (Term))) ((((logic.ifElse) (((equality.equal) ((lists.length) (matchingBindings))) ((1)%Z))) (((inr) ((fun r_ => (binding_term) (r_)) ((lists.head) (matchingBindings)))) : (sum) (Error) (Term))) (((inl) ((Error_Extraction) ((ExtractionError_MultipleBindings) ((Build_MultipleBindingsError) (name))))) : (sum) (Error) (Term)))).
Definition list_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) (Term)) := fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_List v_ => (fun (l : (list) (Term)) => ((inr) (l)) : (sum) (Error) ((list) (Term))) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("list"%string) ((hydra.show.core.term) (stripped)))))) : (sum) (Error) ((list) (Term))
end) (stripped)).
Definition listHead : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term) := fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((list_) (graph_)) (term_))) (fun (l : (list) (Term)) => (((logic.ifElse) ((lists.null) (l))) (((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("non-empty list"%string) ("empty list"%string))))) : (sum) (Error) (Term))) (((inr) ((lists.head) (l))) : (sum) (Error) (Term))).
Definition listOf (t0 : Type) : forall (_ : forall (_ : Term) , (sum) (Error) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) (t0)) := fun (f : forall (_ : Term) , (sum) (Error) (t0)) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((list_) (graph_)) (term_))) (fun (els : (list) (Term)) => ((eithers.mapList) (f)) (els)).
Arguments listOf {t0}.
Definition listType : forall (_ : Type_) , (sum) (Error) (Type_) := fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__List v_ => (fun (t : Type_) => ((inr) (t)) : (sum) (Error) (Type_)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("list type"%string) ((hydra.show.core.type) (typ)))))) : (sum) (Error) (Type_)
end) (stripped).
Definition mapType : forall (_ : Type_) , (sum) (Error) (MapType) := fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Map v_ => (fun (mt : MapType) => ((inr) (mt)) : (sum) (Error) (MapType)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("map type"%string) ((hydra.show.core.type) (typ)))))) : (sum) (Error) (MapType)
end) (stripped).
Definition maybeTerm (t0 : Type) : forall (_ : forall (_ : Term) , (sum) (Error) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((option) (t0)) := fun (f : forall (_ : Term) , (sum) (Error) (t0)) => fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Maybe v_ => (fun (mt : (option) (Term)) => (((maybes.maybe) ((inr) (None))) (fun (t : Term) => ((eithers.map) (maybes.pure)) ((f) (t)))) (mt)) (v_)
| _ => (inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("maybe value"%string) ((hydra.show.core.term) (term_)))))
end) (term_)).
Arguments maybeTerm {t0}.
Definition maybeType : forall (_ : Type_) , (sum) (Error) (Type_) := fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Maybe v_ => (fun (t : Type_) => ((inr) (t)) : (sum) (Error) (Type_)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("maybe type"%string) ((hydra.show.core.type) (typ)))))) : (sum) (Error) (Type_)
end) (stripped).
Definition nArgs (t0 : Type) : forall (_ : Name) , forall (_ : Z) , forall (_ : (list) (t0)) , (sum) (Error) (unit) := fun (name : Name) => fun (n : Z) => fun (args : (list) (t0)) => (((logic.ifElse) (((equality.equal) ((lists.length) (args))) (n))) ((inr) (tt))) ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ((strings.cat) ((cons) ((literals.showInt32) (n)) ((cons) (" arguments to primitive "%string) ((cons) ((literals.showString) ((fun w_ => w_) (name))) (nil))))) ((literals.showInt32) ((lists.length) (args))))))).
Arguments nArgs {t0}.
Definition record_termRecord_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : Name) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) (Field))) (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Record_))) =>
    let record := (fst bundle_) in
    let termRecord := (snd bundle_) in
    (pair (fun (expected : Name) => fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((termRecord) (graph_)) (term0))) (fun (record : Record_) => (((logic.ifElse) (((equality.equal) ((fun r_ => (record__typeName) (r_)) (record))) (expected))) (((inr) ((fun r_ => (record__fields) (r_)) (record))) : (sum) (Error) ((list) (Field)))) (((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) ("record of type "%string)) ((fun w_ => w_) (expected))) ((fun w_ => w_) ((fun r_ => (record__typeName) (r_)) (record))))))) : (sum) (Error) ((list) (Field))))) (fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => ((inr) (record)) : (sum) (Error) (Record_)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("record"%string) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (Record_)
end) (term_))))).

Definition record : forall (_ : Name) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) (Field)) :=
  (fst record_termRecord_bundle).
Definition termRecord : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Record_) :=
  (snd record_termRecord_bundle).
Definition recordType (t0 : Type) : forall (_ : t0) , forall (_ : Type_) , (sum) (Error) ((list) (FieldType)) := fun (ename : t0) => fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Record v_ => (fun (fields : (list) (FieldType)) => (inr) (fields)) (v_)
| _ => (inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("record type"%string) ((hydra.show.core.type) (typ)))))
end) (stripped).
Arguments recordType {t0}.
Definition requireField (t0 : Type) (t1 : Type) (t2 : Type) : forall (_ : string) , forall (_ : forall (_ : t0) , forall (_ : t1) , (sum) (DecodingError) (t2)) , forall (_ : (list) ((prod) (Name) (t1))) , forall (_ : t0) , (sum) (DecodingError) (t2) := fun (fieldName : string) => fun (decoder : forall (_ : t0) , forall (_ : t1) , (sum) (DecodingError) (t2)) => fun (fieldMap : (list) ((prod) (Name) (t1))) => fun (g : t0) => (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("missing field "%string) ((cons) (fieldName) ((cons) (" in record"%string) (nil))))))) (fun (fieldTerm : t1) => ((decoder) (g)) (fieldTerm))) (((maps.lookup) (fieldName)) (fieldMap)).
Arguments requireField {t0} {t1} {t2}.
Definition set : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) (Term)) := fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Set v_ => (fun (s : (list) (Term)) => ((inr) (s)) : (sum) (Error) ((list) (Term))) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("set"%string) ((hydra.show.core.term) (stripped)))))) : (sum) (Error) ((list) (Term))
end) (stripped)).
Definition setOf (t0 : Type) : forall (_ : forall (_ : Term) , (sum) (Error) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) (t0)) := fun (f : forall (_ : Term) , (sum) (Error) (t0)) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) (((set) (graph_)) (term_))) (fun (els : (list) (Term)) => ((eithers.mapSet) (f)) (els)).
Arguments setOf {t0}.
Definition setType : forall (_ : Type_) , (sum) (Error) (Type_) := fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Set v_ => (fun (t : Type_) => ((inr) (t)) : (sum) (Error) (Type_)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("set type"%string) ((hydra.show.core.type) (typ)))))) : (sum) (Error) (Type_)
end) (stripped).
Definition stringLiteral : forall (_ : Literal) , (sum) (Error) (string) := fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (Error) (string)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("string"%string) ((hydra.show.core.literal) (v)))))) : (sum) (Error) (string)
end) (v).
Definition string_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (string) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => (stringLiteral) (l)).
Definition toFieldMap : forall (_ : Record_) , (list) ((prod) (Name) (Term)) := fun (record : Record_) => (maps.fromList) (((lists.map) (fun (f : Field) => (pair) ((fun r_ => (field_name) (r_)) (f)) ((fun r_ => (field_term) (r_)) (f)))) ((fun r_ => (record__fields) (r_)) (record))).
Definition uint16Value : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Uint16 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("uint16"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition uint16 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (uint16Value) (i))).
Definition uint32Value : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Uint32 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("uint32"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition uint32 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (uint32Value) (i))).
Definition uint64Value : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Uint64 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("uint64"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition uint64 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (uint64Value) (i))).
Definition uint8Value : forall (_ : IntegerValue) , (sum) (Error) (Z) := fun (v : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Uint8 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (Error) (Z)) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("uint8"%string) ((integer) (v)))))) : (sum) (Error) (Z)
end) (v).
Definition uint8 : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Z) := fun (graph_ : hydra.graph.Graph) => fun (t : Term) => ((eithers.bind) (((literal) (graph_)) (t))) (fun (l : Literal) => ((eithers.bind) ((integerLiteral) (l))) (fun (i : IntegerValue) => (uint8Value) (i))).
Definition unionType (t0 : Type) : forall (_ : t0) , forall (_ : Type_) , (sum) (Error) ((list) (FieldType)) := fun (ename : t0) => fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Union v_ => (fun (fields : (list) (FieldType)) => (inr) (fields)) (v_)
| _ => (inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("union type"%string) ((hydra.show.core.type) (typ)))))
end) (stripped).
Arguments unionType {t0}.
Definition unit_ : forall (_ : Term) , (sum) (Error) (unit) := fun (term_ : Term) => (fun x_ => match x_ with
| Term_Unit _ => ((inr) (tt)) : (sum) (Error) (unit)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("unit"%string) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (unit)
end) (term_).
Definition unitVariant : forall (_ : Name) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Name) := fun (tname : Name) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) ((((injection) (tname)) (graph_)) (term_))) (fun (field : Field) => ((eithers.bind) ((unit_) ((fun r_ => (field_term) (r_)) (field)))) (fun (ignored : unit) => ((inr) ((fun r_ => (field_name) (r_)) (field))) : (sum) (Error) (Name))).
Definition wrap : forall (_ : Name) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term) := fun (expected : Name) => fun (graph_ : hydra.graph.Graph) => fun (term0 : Term) => ((eithers.bind) (((stripAndDereferenceTerm) (graph_)) (term0))) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => (((logic.ifElse) (((equality.equal) ((fun w_ => w_) ((fun r_ => (wrappedTerm_typeName) (r_)) (wrappedTerm)))) ((fun w_ => w_) (expected)))) (((inr) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm))) : (sum) (Error) (Term))) (((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) ("wrapper of type "%string)) ((fun w_ => w_) (expected))) ((fun w_ => w_) ((fun r_ => (wrappedTerm_typeName) (r_)) (wrappedTerm))))))) : (sum) (Error) (Term))) (v_)
| _ => ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) (((strings.cat2) ("wrap("%string)) ((fun w_ => w_) (expected)))) (")"%string)) ((hydra.show.core.term) (term_)))))) : (sum) (Error) (Term)
end) (term_)).
Definition wrappedType (t0 : Type) : forall (_ : t0) , forall (_ : Type_) , (sum) (Error) (Type_) := fun (ename : t0) => fun (typ : Type_) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Wrap v_ => (fun (innerType : Type_) => (inr) (innerType)) (v_)
| _ => (inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("wrapped type"%string) ((hydra.show.core.type) (typ)))))
end) (stripped).
Arguments wrappedType {t0}.

