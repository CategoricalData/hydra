(* Term decoders for hydra.error.core *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.errors hydra.error.core hydra.lib.eithers hydra.extract.core hydra.decode.paths hydra.paths hydra.decode.core hydra.decode.variants hydra.variants hydra.lib.maps hydra.lib.maybes hydra.lib.strings.

Definition voidInNonBottomPositionError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (VoidInNonBottomPositionError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_VoidInNonBottomPositionError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition untypedTermVariableError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UntypedTermVariableError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UntypedTermVariableError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unnecessaryIdentityApplicationError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UnnecessaryIdentityApplicationError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_UnnecessaryIdentityApplicationError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unknownPrimitiveNameError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UnknownPrimitiveNameError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UnknownPrimitiveNameError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unexpectedTypeVariantError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UnexpectedTypeVariantError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("expectedVariant"%string)) (hydra.decode.variants.typeVariant)) (fieldMap)) (cx))) (fun (field_expectedVariant : TypeVariant) => ((eithers.bind) (((((requireField) ("actualType"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_actualType : Type_) => (inr) ((Build_UnexpectedTypeVariantError) (field_expectedVariant) (field_actualType))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unexpectedTermVariantError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UnexpectedTermVariantError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("expectedVariant"%string)) (hydra.decode.variants.termVariant)) (fieldMap)) (cx))) (fun (field_expectedVariant : TermVariant) => ((eithers.bind) (((((requireField) ("actualTerm"%string)) (hydra.decode.core.term)) (fieldMap)) (cx))) (fun (field_actualTerm : Term) => (inr) ((Build_UnexpectedTermVariantError) (field_expectedVariant) (field_actualTerm))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition undefinedTypeVariableInTypeApplicationError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UndefinedTypeVariableInTypeApplicationError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UndefinedTypeVariableInTypeApplicationError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition undefinedTypeVariableInLambdaDomainError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UndefinedTypeVariableInLambdaDomainError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UndefinedTypeVariableInLambdaDomainError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition undefinedTypeVariableInBindingTypeError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UndefinedTypeVariableInBindingTypeError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UndefinedTypeVariableInBindingTypeError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition undefinedTypeVariableError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UndefinedTypeVariableError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UndefinedTypeVariableError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition undefinedTermVariableError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UndefinedTermVariableError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UndefinedTermVariableError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition undefinedFieldError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (UndefinedFieldError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("fieldName"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_fieldName : Name) => ((eithers.bind) (((((requireField) ("typeName"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_typeName : Name) => (inr) ((Build_UndefinedFieldError) (field_fieldName) (field_typeName))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition typeVariableShadowingInTypeLambdaError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (TypeVariableShadowingInTypeLambdaError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_TypeVariableShadowingInTypeLambdaError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition typeVariableShadowingInForallError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (TypeVariableShadowingInForallError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_TypeVariableShadowingInForallError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition termVariableShadowingError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (TermVariableShadowingError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_TermVariableShadowingError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition singleVariantUnionError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (SingleVariantUnionError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("fieldName"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_fieldName : Name) => (inr) ((Build_SingleVariantUnionError) (field_location) (field_fieldName))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition selfApplicationError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (SelfApplicationError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_SelfApplicationError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition redundantWrapUnwrapError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (RedundantWrapUnwrapError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("typeName"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_typeName : Name) => (inr) ((Build_RedundantWrapUnwrapError) (field_location) (field_typeName))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition nonComparableSetElementTypeError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (NonComparableSetElementTypeError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("elementType"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_elementType : Type_) => (inr) ((Build_NonComparableSetElementTypeError) (field_location) (field_elementType))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition nonComparableMapKeyTypeError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (NonComparableMapKeyTypeError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("keyType"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_keyType : Type_) => (inr) ((Build_NonComparableMapKeyTypeError) (field_location) (field_keyType))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition nestedTypeAnnotationError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (NestedTypeAnnotationError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_NestedTypeAnnotationError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition nestedTermAnnotationError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (NestedTermAnnotationError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_NestedTermAnnotationError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition invalidTypeSchemeVariableNameError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (InvalidTypeSchemeVariableNameError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_InvalidTypeSchemeVariableNameError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition invalidTypeLambdaParameterNameError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (InvalidTypeLambdaParameterNameError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_InvalidTypeLambdaParameterNameError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition invalidLetBindingNameError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (InvalidLetBindingNameError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_InvalidLetBindingNameError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition invalidLambdaParameterNameError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (InvalidLambdaParameterNameError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_InvalidLambdaParameterNameError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition invalidForallParameterNameError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (InvalidForallParameterNameError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_InvalidForallParameterNameError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition emptyUnionTypeError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (EmptyUnionTypeError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_EmptyUnionTypeError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition emptyTypeNameInTermError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (EmptyTypeNameInTermError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_EmptyTypeNameInTermError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition emptyTypeAnnotationError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (EmptyTypeAnnotationError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_EmptyTypeAnnotationError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition emptyTermAnnotationError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (EmptyTermAnnotationError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_EmptyTermAnnotationError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition emptyRecordTypeError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (EmptyRecordTypeError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_EmptyRecordTypeError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition emptyLetBindingsError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (EmptyLetBindingsError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => (inr) ((Build_EmptyLetBindingsError) (field_location)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition emptyCaseStatementError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (EmptyCaseStatementError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("typeName"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_typeName : Name) => (inr) ((Build_EmptyCaseStatementError) (field_location) (field_typeName))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition duplicateUnionTypeFieldNamesError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (DuplicateUnionTypeFieldNamesError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_DuplicateUnionTypeFieldNamesError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition duplicateRecordTypeFieldNamesError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (DuplicateRecordTypeFieldNamesError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_DuplicateRecordTypeFieldNamesError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition invalidTypeError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (InvalidTypeError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Union v_ => (fun (inj : Injection) => let variantMap := (maps.fromList) ((cons) ((pair) ("duplicateRecordTypeFieldNames"%string) (fun (input : Term) => ((eithers.map) (fun (t : DuplicateRecordTypeFieldNamesError) => (InvalidTypeError_DuplicateRecordTypeFieldNames) (t))) (((duplicateRecordTypeFieldNamesError) (cx)) (input)))) ((cons) ((pair) ("duplicateUnionTypeFieldNames"%string) (fun (input : Term) => ((eithers.map) (fun (t : DuplicateUnionTypeFieldNamesError) => (InvalidTypeError_DuplicateUnionTypeFieldNames) (t))) (((duplicateUnionTypeFieldNamesError) (cx)) (input)))) ((cons) ((pair) ("emptyRecordType"%string) (fun (input : Term) => ((eithers.map) (fun (t : EmptyRecordTypeError) => (InvalidTypeError_EmptyRecordType) (t))) (((emptyRecordTypeError) (cx)) (input)))) ((cons) ((pair) ("emptyTypeAnnotation"%string) (fun (input : Term) => ((eithers.map) (fun (t : EmptyTypeAnnotationError) => (InvalidTypeError_EmptyTypeAnnotation) (t))) (((emptyTypeAnnotationError) (cx)) (input)))) ((cons) ((pair) ("emptyUnionType"%string) (fun (input : Term) => ((eithers.map) (fun (t : EmptyUnionTypeError) => (InvalidTypeError_EmptyUnionType) (t))) (((emptyUnionTypeError) (cx)) (input)))) ((cons) ((pair) ("invalidForallParameterName"%string) (fun (input : Term) => ((eithers.map) (fun (t : InvalidForallParameterNameError) => (InvalidTypeError_InvalidForallParameterName) (t))) (((invalidForallParameterNameError) (cx)) (input)))) ((cons) ((pair) ("invalidTypeSchemeVariableName"%string) (fun (input : Term) => ((eithers.map) (fun (t : InvalidTypeSchemeVariableNameError) => (InvalidTypeError_InvalidTypeSchemeVariableName) (t))) (((invalidTypeSchemeVariableNameError) (cx)) (input)))) ((cons) ((pair) ("nestedTypeAnnotation"%string) (fun (input : Term) => ((eithers.map) (fun (t : NestedTypeAnnotationError) => (InvalidTypeError_NestedTypeAnnotation) (t))) (((nestedTypeAnnotationError) (cx)) (input)))) ((cons) ((pair) ("nonComparableMapKeyType"%string) (fun (input : Term) => ((eithers.map) (fun (t : NonComparableMapKeyTypeError) => (InvalidTypeError_NonComparableMapKeyType) (t))) (((nonComparableMapKeyTypeError) (cx)) (input)))) ((cons) ((pair) ("nonComparableSetElementType"%string) (fun (input : Term) => ((eithers.map) (fun (t : NonComparableSetElementTypeError) => (InvalidTypeError_NonComparableSetElementType) (t))) (((nonComparableSetElementTypeError) (cx)) (input)))) ((cons) ((pair) ("singleVariantUnion"%string) (fun (input : Term) => ((eithers.map) (fun (t : SingleVariantUnionError) => (InvalidTypeError_SingleVariantUnion) (t))) (((singleVariantUnionError) (cx)) (input)))) ((cons) ((pair) ("typeVariableShadowingInForall"%string) (fun (input : Term) => ((eithers.map) (fun (t : TypeVariableShadowingInForallError) => (InvalidTypeError_TypeVariableShadowingInForall) (t))) (((typeVariableShadowingInForallError) (cx)) (input)))) ((cons) ((pair) ("undefinedTypeVariable"%string) (fun (input : Term) => ((eithers.map) (fun (t : UndefinedTypeVariableError) => (InvalidTypeError_UndefinedTypeVariable) (t))) (((undefinedTypeVariableError) (cx)) (input)))) ((cons) ((pair) ("voidInNonBottomPosition"%string) (fun (input : Term) => ((eithers.map) (fun (t : VoidInNonBottomPositionError) => (InvalidTypeError_VoidInNonBottomPosition) (t))) (((voidInNonBottomPositionError) (cx)) (input)))) (nil))))))))))))))) in let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : Term -> (sum) (DecodingError) (InvalidTypeError)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition duplicateFieldError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (DuplicateFieldError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_DuplicateFieldError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition duplicateBindingError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (DuplicateBindingError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_DuplicateBindingError) (field_location) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition constantConditionError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (ConstantConditionError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("location"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_location : SubtermPath) => ((eithers.bind) (((((requireField) ("value"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Boolean v_ => (fun (b : bool) => (inr) (b)) (v_)
| _ => (inl) ("expected boolean literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_value : bool) => (inr) ((Build_ConstantConditionError) (field_location) (field_value))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition invalidTermError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (InvalidTermError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Union v_ => (fun (inj : Injection) => let variantMap := (maps.fromList) ((cons) ((pair) ("constantCondition"%string) (fun (input : Term) => ((eithers.map) (fun (t : ConstantConditionError) => (InvalidTermError_ConstantCondition) (t))) (((constantConditionError) (cx)) (input)))) ((cons) ((pair) ("duplicateBinding"%string) (fun (input : Term) => ((eithers.map) (fun (t : DuplicateBindingError) => (InvalidTermError_DuplicateBinding) (t))) (((duplicateBindingError) (cx)) (input)))) ((cons) ((pair) ("duplicateField"%string) (fun (input : Term) => ((eithers.map) (fun (t : DuplicateFieldError) => (InvalidTermError_DuplicateField) (t))) (((duplicateFieldError) (cx)) (input)))) ((cons) ((pair) ("emptyCaseStatement"%string) (fun (input : Term) => ((eithers.map) (fun (t : EmptyCaseStatementError) => (InvalidTermError_EmptyCaseStatement) (t))) (((emptyCaseStatementError) (cx)) (input)))) ((cons) ((pair) ("emptyLetBindings"%string) (fun (input : Term) => ((eithers.map) (fun (t : EmptyLetBindingsError) => (InvalidTermError_EmptyLetBindings) (t))) (((emptyLetBindingsError) (cx)) (input)))) ((cons) ((pair) ("emptyTermAnnotation"%string) (fun (input : Term) => ((eithers.map) (fun (t : EmptyTermAnnotationError) => (InvalidTermError_EmptyTermAnnotation) (t))) (((emptyTermAnnotationError) (cx)) (input)))) ((cons) ((pair) ("emptyTypeNameInTerm"%string) (fun (input : Term) => ((eithers.map) (fun (t : EmptyTypeNameInTermError) => (InvalidTermError_EmptyTypeNameInTerm) (t))) (((emptyTypeNameInTermError) (cx)) (input)))) ((cons) ((pair) ("invalidLambdaParameterName"%string) (fun (input : Term) => ((eithers.map) (fun (t : InvalidLambdaParameterNameError) => (InvalidTermError_InvalidLambdaParameterName) (t))) (((invalidLambdaParameterNameError) (cx)) (input)))) ((cons) ((pair) ("invalidLetBindingName"%string) (fun (input : Term) => ((eithers.map) (fun (t : InvalidLetBindingNameError) => (InvalidTermError_InvalidLetBindingName) (t))) (((invalidLetBindingNameError) (cx)) (input)))) ((cons) ((pair) ("invalidTypeLambdaParameterName"%string) (fun (input : Term) => ((eithers.map) (fun (t : InvalidTypeLambdaParameterNameError) => (InvalidTermError_InvalidTypeLambdaParameterName) (t))) (((invalidTypeLambdaParameterNameError) (cx)) (input)))) ((cons) ((pair) ("nestedTermAnnotation"%string) (fun (input : Term) => ((eithers.map) (fun (t : NestedTermAnnotationError) => (InvalidTermError_NestedTermAnnotation) (t))) (((nestedTermAnnotationError) (cx)) (input)))) ((cons) ((pair) ("redundantWrapUnwrap"%string) (fun (input : Term) => ((eithers.map) (fun (t : RedundantWrapUnwrapError) => (InvalidTermError_RedundantWrapUnwrap) (t))) (((redundantWrapUnwrapError) (cx)) (input)))) ((cons) ((pair) ("selfApplication"%string) (fun (input : Term) => ((eithers.map) (fun (t : SelfApplicationError) => (InvalidTermError_SelfApplication) (t))) (((selfApplicationError) (cx)) (input)))) ((cons) ((pair) ("termVariableShadowing"%string) (fun (input : Term) => ((eithers.map) (fun (t : TermVariableShadowingError) => (InvalidTermError_TermVariableShadowing) (t))) (((termVariableShadowingError) (cx)) (input)))) ((cons) ((pair) ("typeVariableShadowingInTypeLambda"%string) (fun (input : Term) => ((eithers.map) (fun (t : TypeVariableShadowingInTypeLambdaError) => (InvalidTermError_TypeVariableShadowingInTypeLambda) (t))) (((typeVariableShadowingInTypeLambdaError) (cx)) (input)))) ((cons) ((pair) ("undefinedTermVariable"%string) (fun (input : Term) => ((eithers.map) (fun (t : UndefinedTermVariableError) => (InvalidTermError_UndefinedTermVariable) (t))) (((undefinedTermVariableError) (cx)) (input)))) ((cons) ((pair) ("undefinedTypeVariableInBindingType"%string) (fun (input : Term) => ((eithers.map) (fun (t : UndefinedTypeVariableInBindingTypeError) => (InvalidTermError_UndefinedTypeVariableInBindingType) (t))) (((undefinedTypeVariableInBindingTypeError) (cx)) (input)))) ((cons) ((pair) ("undefinedTypeVariableInLambdaDomain"%string) (fun (input : Term) => ((eithers.map) (fun (t : UndefinedTypeVariableInLambdaDomainError) => (InvalidTermError_UndefinedTypeVariableInLambdaDomain) (t))) (((undefinedTypeVariableInLambdaDomainError) (cx)) (input)))) ((cons) ((pair) ("undefinedTypeVariableInTypeApplication"%string) (fun (input : Term) => ((eithers.map) (fun (t : UndefinedTypeVariableInTypeApplicationError) => (InvalidTermError_UndefinedTypeVariableInTypeApplication) (t))) (((undefinedTypeVariableInTypeApplicationError) (cx)) (input)))) ((cons) ((pair) ("unknownPrimitiveName"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnknownPrimitiveNameError) => (InvalidTermError_UnknownPrimitiveName) (t))) (((unknownPrimitiveNameError) (cx)) (input)))) ((cons) ((pair) ("unnecessaryIdentityApplication"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnnecessaryIdentityApplicationError) => (InvalidTermError_UnnecessaryIdentityApplication) (t))) (((unnecessaryIdentityApplicationError) (cx)) (input)))) ((cons) ((pair) ("untypedTermVariable"%string) (fun (input : Term) => ((eithers.map) (fun (t : UntypedTermVariableError) => (InvalidTermError_UntypedTermVariable) (t))) (((untypedTermVariableError) (cx)) (input)))) (nil))))))))))))))))))))))) in let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : Term -> (sum) (DecodingError) (InvalidTermError)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

