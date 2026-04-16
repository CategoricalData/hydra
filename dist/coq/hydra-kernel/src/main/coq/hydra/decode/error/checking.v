(* Term decoders for hydra.error.checking *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.decode.core hydra.decode.paths hydra.decode.typing hydra.decode.variants hydra.error.checking hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings hydra.paths hydra.typing hydra.variants.

Definition incorrectUnificationError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (IncorrectUnificationError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("substitution"%string)) (hydra.decode.typing.typeSubst)) (fieldMap)) (cx))) (fun (field_substitution : TypeSubst) => (inr) ((Build_IncorrectUnificationError) (field_substitution)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition notAForallTypeError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (NotAForallTypeError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("type"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_type : Type_) => ((eithers.bind) (((((requireField) ("typeArguments"%string)) ((decodeList) (hydra.decode.core.type))) (fieldMap)) (cx))) (fun (field_typeArguments : (list) (Type_)) => (inr) ((Build_NotAForallTypeError) (field_type) (field_typeArguments))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition notAFunctionTypeError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (NotAFunctionTypeError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("type"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_type : Type_) => (inr) ((Build_NotAFunctionTypeError) (field_type)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition otherCheckingError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (OtherCheckingError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("path"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_path : SubtermPath) => ((eithers.bind) (((((requireField) ("message"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_message : string) => (inr) ((Build_OtherCheckingError) (field_path) (field_message))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition typeArityMismatchError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeArityMismatchError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("type"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_type : Type_) => ((eithers.bind) (((((requireField) ("expectedArity"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_expectedArity : Z) => ((eithers.bind) (((((requireField) ("actualArity"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_actualArity : Z) => ((eithers.bind) (((((requireField) ("typeArguments"%string)) ((decodeList) (hydra.decode.core.type))) (fieldMap)) (cx))) (fun (field_typeArguments : (list) (Type_)) => (inr) ((Build_TypeArityMismatchError) (field_type) (field_expectedArity) (field_actualArity) (field_typeArguments))))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition typeMismatchError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeMismatchError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("expectedType"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_expectedType : Type_) => ((eithers.bind) (((((requireField) ("actualType"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_actualType : Type_) => (inr) ((Build_TypeMismatchError) (field_expectedType) (field_actualType))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unboundTypeVariablesError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UnboundTypeVariablesError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("variables"%string)) ((decodeSet) (hydra.decode.core.name))) (fieldMap)) (cx))) (fun (field_variables : (list) (Name)) => ((eithers.bind) (((((requireField) ("type"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_type : Type_) => (inr) ((Build_UnboundTypeVariablesError) (field_variables) (field_type))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition undefinedTermVariableCheckingError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UndefinedTermVariableCheckingError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("path"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_path : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UndefinedTermVariableCheckingError) (field_path) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unequalTypesError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UnequalTypesError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("types"%string)) ((decodeList) (hydra.decode.core.type))) (fieldMap)) (cx))) (fun (field_types : (list) (Type_)) => ((eithers.bind) (((((requireField) ("description"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_description : string) => (inr) ((Build_UnequalTypesError) (field_types) (field_description))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition unsupportedTermVariantError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UnsupportedTermVariantError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("termVariant"%string)) (hydra.decode.variants.termVariant)) (fieldMap)) (cx))) (fun (field_termVariant : TermVariant) => (inr) ((Build_UnsupportedTermVariantError) (field_termVariant)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition untypedLambdaError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UntypedLambdaError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in (inr) (tt)) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition untypedLetBindingError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UntypedLetBindingError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("binding"%string)) (hydra.decode.core.binding)) (fieldMap)) (cx))) (fun (field_binding : Binding) => (inr) ((Build_UntypedLetBindingError) (field_binding)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition untypedTermVariableCheckingError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UntypedTermVariableCheckingError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("path"%string)) (hydra.decode.paths.subtermPath)) (fieldMap)) (cx))) (fun (field_path : SubtermPath) => ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => (inr) ((Build_UntypedTermVariableCheckingError) (field_path) (field_name))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition checkingError : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (CheckingError) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("incorrectUnification"%string) (fun (input : Term) => ((eithers.map) (fun (t : IncorrectUnificationError) => (CheckingError_IncorrectUnification) (t))) (((incorrectUnificationError) (cx)) (input)))) ((cons) ((pair) ("notAForallType"%string) (fun (input : Term) => ((eithers.map) (fun (t : NotAForallTypeError) => (CheckingError_NotAForallType) (t))) (((notAForallTypeError) (cx)) (input)))) ((cons) ((pair) ("notAFunctionType"%string) (fun (input : Term) => ((eithers.map) (fun (t : NotAFunctionTypeError) => (CheckingError_NotAFunctionType) (t))) (((notAFunctionTypeError) (cx)) (input)))) ((cons) ((pair) ("other"%string) (fun (input : Term) => ((eithers.map) (fun (t : OtherCheckingError) => (CheckingError_Other) (t))) (((otherCheckingError) (cx)) (input)))) ((cons) ((pair) ("typeArityMismatch"%string) (fun (input : Term) => ((eithers.map) (fun (t : TypeArityMismatchError) => (CheckingError_TypeArityMismatch) (t))) (((typeArityMismatchError) (cx)) (input)))) ((cons) ((pair) ("typeMismatch"%string) (fun (input : Term) => ((eithers.map) (fun (t : TypeMismatchError) => (CheckingError_TypeMismatch) (t))) (((typeMismatchError) (cx)) (input)))) ((cons) ((pair) ("unboundTypeVariables"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnboundTypeVariablesError) => (CheckingError_UnboundTypeVariables) (t))) (((unboundTypeVariablesError) (cx)) (input)))) ((cons) ((pair) ("undefinedTermVariable"%string) (fun (input : Term) => ((eithers.map) (fun (t : UndefinedTermVariableCheckingError) => (CheckingError_UndefinedTermVariable) (t))) (((undefinedTermVariableCheckingError) (cx)) (input)))) ((cons) ((pair) ("unequalTypes"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnequalTypesError) => (CheckingError_UnequalTypes) (t))) (((unequalTypesError) (cx)) (input)))) ((cons) ((pair) ("unsupportedTermVariant"%string) (fun (input : Term) => ((eithers.map) (fun (t : UnsupportedTermVariantError) => (CheckingError_UnsupportedTermVariant) (t))) (((unsupportedTermVariantError) (cx)) (input)))) ((cons) ((pair) ("untypedLambda"%string) (fun (input : Term) => ((eithers.map) (fun (t : UntypedLambdaError) => (CheckingError_UntypedLambda) (t))) (((untypedLambdaError) (cx)) (input)))) ((cons) ((pair) ("untypedLetBinding"%string) (fun (input : Term) => ((eithers.map) (fun (t : UntypedLetBindingError) => (CheckingError_UntypedLetBinding) (t))) (((untypedLetBindingError) (cx)) (input)))) ((cons) ((pair) ("untypedTermVariable"%string) (fun (input : Term) => ((eithers.map) (fun (t : UntypedTermVariableCheckingError) => (CheckingError_UntypedTermVariable) (t))) (((untypedTermVariableCheckingError) (cx)) (input)))) (nil)))))))))))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (CheckingError)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

