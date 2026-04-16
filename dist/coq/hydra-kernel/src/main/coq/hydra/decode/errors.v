(* Term decoders for hydra.errors *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.decode.core hydra.decode.error.checking hydra.decode.error.core hydra.decode.paths hydra.error.checking hydra.error.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings hydra.paths.

Definition decodingError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (DecodingError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition emptyListError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (unit) := fun (cx : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx)) (t).
Definition multipleBindingsError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (MultipleBindingsError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_MultipleBindingsError) (field_name)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition multipleFieldsError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (MultipleFieldsError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("fieldName"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_fieldName : Name) => (inr) ((Build_MultipleFieldsError) (field_fieldName)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition noMatchingFieldError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (NoMatchingFieldError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("fieldName"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_fieldName : Name) => (inr) ((Build_NoMatchingFieldError) (field_fieldName)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition noSuchBindingError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (NoSuchBindingError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_NoSuchBindingError) (field_name)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition notEnoughCasesError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (unit) := fun (cx : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx)) (t).
Definition unexpectedShapeError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UnexpectedShapeError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("expected"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_expected : string) => ((eithers.bind) (((((requireField) ("actual"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_actual : string) => (inr) ((Build_UnexpectedShapeError) (field_expected) (field_actual))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition extractionError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ExtractionError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("emptyList"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (ExtractionError_EmptyList) (t))) (((emptyListError) (cx)) (input)))) ((cons) ((pair) ("multipleBindings"%string) (fun (input : Term) => ((eithers.map) (fun (t : MultipleBindingsError) => (ExtractionError_MultipleBindings) (t))) (((multipleBindingsError) (cx)) (input)))) ((cons) ((pair) ("multipleFields"%string) (fun (input : Term) => ((eithers.map) (fun (t : MultipleFieldsError) => (ExtractionError_MultipleFields) (t))) (((multipleFieldsError) (cx)) (input)))) ((cons) ((pair) ("noMatchingField"%string) (fun (input : Term) => ((eithers.map) (fun (t : NoMatchingFieldError) => (ExtractionError_NoMatchingField) (t))) (((noMatchingFieldError) (cx)) (input)))) ((cons) ((pair) ("noSuchBinding"%string) (fun (input : Term) => ((eithers.map) (fun (t : NoSuchBindingError) => (ExtractionError_NoSuchBinding) (t))) (((noSuchBindingError) (cx)) (input)))) ((cons) ((pair) ("notEnoughCases"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (ExtractionError_NotEnoughCases) (t))) (((notEnoughCasesError) (cx)) (input)))) ((cons) ((pair) ("unexpectedShape"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnexpectedShapeError) => (ExtractionError_UnexpectedShape) (t))) (((unexpectedShapeError) (cx)) (input)))) (nil)))))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (ExtractionError)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition otherInferenceError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (OtherInferenceError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("path"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_path : SubtermPath) => ((eithers.bind) (((((requireField) ("message"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_message : string) => (inr) ((Build_OtherInferenceError) (field_path) (field_message))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unificationError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UnificationError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("leftType"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_leftType : Type_) => ((eithers.bind) (((((requireField) ("rightType"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_rightType : Type_) => ((eithers.bind) (((((requireField) ("message"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_message : string) => (inr) ((Build_UnificationError) (field_leftType) (field_rightType) (field_message)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unificationInferenceError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UnificationInferenceError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("path"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_path : SubtermPath) => ((eithers.bind) (((((requireField) ("cause"%string)) (unificationError)) (fieldMap)) (cx))) (fun (field_cause : UnificationError) => (inr) ((Build_UnificationInferenceError) (field_path) (field_cause))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition inferenceError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (InferenceError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("checking"%string) (fun (input : Term) => ((eithers.map) (fun (t : CheckingError) => (InferenceError_Checking) (t))) (((hydra.decode.error.checking.checkingError) (cx)) (input)))) ((cons) ((pair) ("other"%string) (fun (input : Term) => ((eithers.map) (fun (t : OtherInferenceError) => (InferenceError_Other) (t))) (((otherInferenceError) (cx)) (input)))) ((cons) ((pair) ("unification"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnificationInferenceError) => (InferenceError_Unification) (t))) (((unificationInferenceError) (cx)) (input)))) (nil)))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (InferenceError)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition otherError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (OtherError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition noSuchPrimitiveError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (NoSuchPrimitiveError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_NoSuchPrimitiveError) (field_name)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition otherResolutionError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (OtherResolutionError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition resolutionError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ResolutionError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("noSuchBinding"%string) (fun (input : Term) => ((eithers.map) (fun (t : NoSuchBindingError) => (ResolutionError_NoSuchBinding) (t))) (((noSuchBindingError) (cx)) (input)))) ((cons) ((pair) ("noSuchPrimitive"%string) (fun (input : Term) => ((eithers.map) (fun (t : NoSuchPrimitiveError) => (ResolutionError_NoSuchPrimitive) (t))) (((noSuchPrimitiveError) (cx)) (input)))) ((cons) ((pair) ("noMatchingField"%string) (fun (input : Term) => ((eithers.map) (fun (t : NoMatchingFieldError) => (ResolutionError_NoMatchingField) (t))) (((noMatchingFieldError) (cx)) (input)))) ((cons) ((pair) ("other"%string) (fun (input : Term) => ((eithers.map) (fun (t : OtherResolutionError) => (ResolutionError_Other) (t))) (((otherResolutionError) (cx)) (input)))) ((cons) ((pair) ("unexpectedShape"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnexpectedShapeError) => (ResolutionError_UnexpectedShape) (t))) (((unexpectedShapeError) (cx)) (input)))) (nil)))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (ResolutionError)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition error : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Error) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("checking"%string) (fun (input : Term) => ((eithers.map) (fun (t : CheckingError) => (Error_Checking) (t))) (((hydra.decode.error.checking.checkingError) (cx)) (input)))) ((cons) ((pair) ("decoding"%string) (fun (input : Term) => ((eithers.map) (fun (t : DecodingError) => (Error_Decoding) (t))) (((decodingError) (cx)) (input)))) ((cons) ((pair) ("duplicateBinding"%string) (fun (input : Term) => ((eithers.map) (fun (t : DuplicateBindingError) => (Error_DuplicateBinding) (t))) (((hydra.decode.error.core.duplicateBindingError) (cx)) (input)))) ((cons) ((pair) ("duplicateField"%string) (fun (input : Term) => ((eithers.map) (fun (t : DuplicateFieldError) => (Error_DuplicateField) (t))) (((hydra.decode.error.core.duplicateFieldError) (cx)) (input)))) ((cons) ((pair) ("extraction"%string) (fun (input : Term) => ((eithers.map) (fun (t : ExtractionError) => (Error_Extraction) (t))) (((extractionError) (cx)) (input)))) ((cons) ((pair) ("inference"%string) (fun (input : Term) => ((eithers.map) (fun (t : InferenceError) => (Error_Inference) (t))) (((inferenceError) (cx)) (input)))) ((cons) ((pair) ("other"%string) (fun (input : Term) => ((eithers.map) (fun (t : OtherError) => (Error_Other) (t))) (((otherError) (cx)) (input)))) ((cons) ((pair) ("resolution"%string) (fun (input : Term) => ((eithers.map) (fun (t : ResolutionError) => (Error_Resolution) (t))) (((resolutionError) (cx)) (input)))) ((cons) ((pair) ("undefinedField"%string) (fun (input : Term) => ((eithers.map) (fun (t : UndefinedFieldError) => (Error_UndefinedField) (t))) (((hydra.decode.error.core.undefinedFieldError) (cx)) (input)))) ((cons) ((pair) ("undefinedTermVariable"%string) (fun (input : Term) => ((eithers.map) (fun (t : UndefinedTermVariableError) => (Error_UndefinedTermVariable) (t))) (((hydra.decode.error.core.undefinedTermVariableError) (cx)) (input)))) ((cons) ((pair) ("untypedTermVariable"%string) (fun (input : Term) => ((eithers.map) (fun (t : UntypedTermVariableError) => (Error_UntypedTermVariable) (t))) (((hydra.decode.error.core.untypedTermVariableError) (cx)) (input)))) ((cons) ((pair) ("unexpectedTermVariant"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnexpectedTermVariantError) => (Error_UnexpectedTermVariant) (t))) (((hydra.decode.error.core.unexpectedTermVariantError) (cx)) (input)))) ((cons) ((pair) ("unexpectedTypeVariant"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnexpectedTypeVariantError) => (Error_UnexpectedTypeVariant) (t))) (((hydra.decode.error.core.unexpectedTypeVariantError) (cx)) (input)))) ((cons) ((pair) ("unification"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnificationError) => (Error_Unification) (t))) (((unificationError) (cx)) (input)))) (nil))))))))))))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Error)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

