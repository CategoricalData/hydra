(* Term decoders for hydra.core *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings.

Definition annotatedTerm_annotatedType_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (AnnotatedTerm)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (AnnotatedType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Application)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ApplicationType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Binding)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (CaseStatement)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (EitherType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Field)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FieldType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FloatType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FloatValue)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ForallType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FunctionType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Injection)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (IntegerType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (IntegerValue)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Lambda)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Let)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Literal)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (LiteralType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (MapType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Name)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (PairType)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Projection)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Record_)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Term)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Type_)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeApplicationTerm)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeLambda)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeScheme)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeVariableMetadata)) (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (WrappedTerm))))))))))))))))))))))))))))))))) =>
    let annotatedTerm := (fst bundle_) in
    let annotatedType := (fst (snd bundle_)) in
    let application := (fst (snd (snd bundle_))) in
    let applicationType := (fst (snd (snd (snd bundle_)))) in
    let binding := (fst (snd (snd (snd (snd bundle_))))) in
    let caseStatement := (fst (snd (snd (snd (snd (snd bundle_)))))) in
    let eitherType := (fst (snd (snd (snd (snd (snd (snd bundle_))))))) in
    let field := (fst (snd (snd (snd (snd (snd (snd (snd bundle_)))))))) in
    let fieldType := (fst (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))) in
    let floatType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))) in
    let floatValue := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))) in
    let forallType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))) in
    let functionType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))) in
    let injection := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))) in
    let integerType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))) in
    let integerValue := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))) in
    let lambda := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))) in
    let let_ := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))) in
    let literal := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))) in
    let literalType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))) in
    let mapType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))))) in
    let name := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))))) in
    let pairType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))))))) in
    let projection := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))))))) in
    let record := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))))))))) in
    let term := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))))))))) in
    let type := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))))))))))) in
    let typeApplicationTerm := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))))))))))) in
    let typeLambda := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))))))))))))) in
    let typeScheme := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))))))))))))) in
    let typeVariableMetadata := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))))))))))))))) in
    let wrappedTerm := (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))))))))))))))) in
    (pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("body"%string)) (term)) (fieldMap)) (cx))) (fun (field_body : Term) => ((eithers.bind) (((((requireField) ("annotation"%string)) (((decodeMap) (name)) (term))) (fieldMap)) (cx))) (fun (field_annotation : (list) ((prod) (Name) (Term))) => (inr) ((Build_AnnotatedTerm) (field_body) (field_annotation))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("body"%string)) (type)) (fieldMap)) (cx))) (fun (field_body : Type_) => ((eithers.bind) (((((requireField) ("annotation"%string)) (((decodeMap) (name)) (term))) (fieldMap)) (cx))) (fun (field_annotation : (list) ((prod) (Name) (Term))) => (inr) ((Build_AnnotatedType) (field_body) (field_annotation))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("function"%string)) (term)) (fieldMap)) (cx))) (fun (field_function : Term) => ((eithers.bind) (((((requireField) ("argument"%string)) (term)) (fieldMap)) (cx))) (fun (field_argument : Term) => (inr) ((Build_Application) (field_function) (field_argument))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("function"%string)) (type)) (fieldMap)) (cx))) (fun (field_function : Type_) => ((eithers.bind) (((((requireField) ("argument"%string)) (type)) (fieldMap)) (cx))) (fun (field_argument : Type_) => (inr) ((Build_ApplicationType) (field_function) (field_argument))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("term"%string)) (term)) (fieldMap)) (cx))) (fun (field_term : Term) => ((eithers.bind) (((((requireField) ("type"%string)) ((decodeMaybe) (typeScheme))) (fieldMap)) (cx))) (fun (field_type : (option) (TypeScheme)) => (inr) ((Build_Binding) (field_name) (field_term) (field_type)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("typeName"%string)) (name)) (fieldMap)) (cx))) (fun (field_typeName : Name) => ((eithers.bind) (((((requireField) ("default"%string)) ((decodeMaybe) (term))) (fieldMap)) (cx))) (fun (field_default : (option) (Term)) => ((eithers.bind) (((((requireField) ("cases"%string)) ((decodeList) (field))) (fieldMap)) (cx))) (fun (field_cases : (list) (Field)) => (inr) ((Build_CaseStatement) (field_typeName) (field_default) (field_cases)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("left"%string)) (type)) (fieldMap)) (cx))) (fun (field_left : Type_) => ((eithers.bind) (((((requireField) ("right"%string)) (type)) (fieldMap)) (cx))) (fun (field_right : Type_) => (inr) ((Build_EitherType) (field_left) (field_right))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("term"%string)) (term)) (fieldMap)) (cx))) (fun (field_term : Term) => (inr) ((Build_Field) (field_name) (field_term))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("type"%string)) (type)) (fieldMap)) (cx))) (fun (field_type : Type_) => (inr) ((Build_FieldType) (field_name) (field_type))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("bigfloat"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (FloatType_Bigfloat) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("float32"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (FloatType_Float32) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("float64"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (FloatType_Float64) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil)))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (FloatType)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("bigfloat"%string) (fun (input : Term) => ((eithers.map) (fun (t : Q) => (FloatValue_Bigfloat) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Float v_ => (fun x_ => match x_ with
| FloatValue_Bigfloat v_ => (fun (f : Q) => (inr) (f)) (v_)
| _ => (inl) ("expected bigfloat value"%string)
end) (v_)
| _ => (inl) ("expected bigfloat literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("float32"%string) (fun (input : Term) => ((eithers.map) (fun (t : Q) => (FloatValue_Float32) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Float v_ => (fun x_ => match x_ with
| FloatValue_Float32 v_ => (fun (f : Q) => (inr) (f)) (v_)
| _ => (inl) ("expected float32 value"%string)
end) (v_)
| _ => (inl) ("expected float32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("float64"%string) (fun (input : Term) => ((eithers.map) (fun (t : Q) => (FloatValue_Float64) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Float v_ => (fun x_ => match x_ with
| FloatValue_Float64 v_ => (fun (f : Q) => (inr) (f)) (v_)
| _ => (inl) ("expected float64 value"%string)
end) (v_)
| _ => (inl) ("expected float64 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) (nil)))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (FloatValue)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("parameter"%string)) (name)) (fieldMap)) (cx))) (fun (field_parameter : Name) => ((eithers.bind) (((((requireField) ("body"%string)) (type)) (fieldMap)) (cx))) (fun (field_body : Type_) => (inr) ((Build_ForallType) (field_parameter) (field_body))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("domain"%string)) (type)) (fieldMap)) (cx))) (fun (field_domain : Type_) => ((eithers.bind) (((((requireField) ("codomain"%string)) (type)) (fieldMap)) (cx))) (fun (field_codomain : Type_) => (inr) ((Build_FunctionType) (field_domain) (field_codomain))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("typeName"%string)) (name)) (fieldMap)) (cx))) (fun (field_typeName : Name) => ((eithers.bind) (((((requireField) ("field"%string)) (field)) (fieldMap)) (cx))) (fun (field_field : Field) => (inr) ((Build_Injection) (field_typeName) (field_field))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("bigint"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Bigint) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("int8"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Int8) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("int16"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Int16) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("int32"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Int32) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("int64"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Int64) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("uint8"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Uint8) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("uint16"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Uint16) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("uint32"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Uint32) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("uint64"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (IntegerType_Uint64) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil)))))))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (IntegerType)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("bigint"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Bigint) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Bigint v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected bigint value"%string)
end) (v_)
| _ => (inl) ("expected bigint literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("int8"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Int8) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int8 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int8 value"%string)
end) (v_)
| _ => (inl) ("expected int8 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("int16"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Int16) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int16 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int16 value"%string)
end) (v_)
| _ => (inl) ("expected int16 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("int32"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Int32) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("int64"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Int64) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int64 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int64 value"%string)
end) (v_)
| _ => (inl) ("expected int64 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("uint8"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Uint8) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Uint8 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected uint8 value"%string)
end) (v_)
| _ => (inl) ("expected uint8 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("uint16"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Uint16) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Uint16 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected uint16 value"%string)
end) (v_)
| _ => (inl) ("expected uint16 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("uint32"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Uint32) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Uint32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected uint32 value"%string)
end) (v_)
| _ => (inl) ("expected uint32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("uint64"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (IntegerValue_Uint64) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Uint64 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected uint64 value"%string)
end) (v_)
| _ => (inl) ("expected uint64 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) (nil)))))))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (IntegerValue)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("parameter"%string)) (name)) (fieldMap)) (cx))) (fun (field_parameter : Name) => ((eithers.bind) (((((requireField) ("domain"%string)) ((decodeMaybe) (type))) (fieldMap)) (cx))) (fun (field_domain : (option) (Type_)) => ((eithers.bind) (((((requireField) ("body"%string)) (term)) (fieldMap)) (cx))) (fun (field_body : Term) => (inr) ((Build_Lambda) (field_parameter) (field_domain) (field_body)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("bindings"%string)) ((decodeList) (binding))) (fieldMap)) (cx))) (fun (field_bindings : (list) (Binding)) => ((eithers.bind) (((((requireField) ("body"%string)) (term)) (fieldMap)) (cx))) (fun (field_body : Term) => (inr) ((Build_Let) (field_bindings) (field_body))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("binary"%string) (fun (input : Term) => ((eithers.map) (fun (t : string) => (Literal_Binary) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Binary v_ => (fun (b : string) => (inr) (b)) (v_)
| _ => (inl) ("expected binary literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("boolean"%string) (fun (input : Term) => ((eithers.map) (fun (t : bool) => (Literal_Boolean) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Boolean v_ => (fun (b : bool) => (inr) (b)) (v_)
| _ => (inl) ("expected boolean literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("float"%string) (fun (input : Term) => ((eithers.map) (fun (t : FloatValue) => (Literal_Float) (t))) (((floatValue) (cx)) (input)))) ((cons) ((pair) ("integer"%string) (fun (input : Term) => ((eithers.map) (fun (t : IntegerValue) => (Literal_Integer) (t))) (((integerValue) (cx)) (input)))) ((cons) ((pair) ("string"%string) (fun (input : Term) => ((eithers.map) (fun (t : string) => (Literal_String) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) (nil)))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Literal)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("binary"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (LiteralType_Binary) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("boolean"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (LiteralType_Boolean) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("float"%string) (fun (input : Term) => ((eithers.map) (fun (t : FloatType) => (LiteralType_Float) (t))) (((floatType) (cx)) (input)))) ((cons) ((pair) ("integer"%string) (fun (input : Term) => ((eithers.map) (fun (t : IntegerType) => (LiteralType_Integer) (t))) (((integerType) (cx)) (input)))) ((cons) ((pair) ("string"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (LiteralType_String) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil)))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (LiteralType)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("keys"%string)) (type)) (fieldMap)) (cx))) (fun (field_keys : Type_) => ((eithers.bind) (((((requireField) ("values"%string)) (type)) (fieldMap)) (cx))) (fun (field_values : Type_) => (inr) ((Build_MapType) (field_keys) (field_values))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("first"%string)) (type)) (fieldMap)) (cx))) (fun (field_first : Type_) => ((eithers.bind) (((((requireField) ("second"%string)) (type)) (fieldMap)) (cx))) (fun (field_second : Type_) => (inr) ((Build_PairType) (field_first) (field_second))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("typeName"%string)) (name)) (fieldMap)) (cx))) (fun (field_typeName : Name) => ((eithers.bind) (((((requireField) ("field"%string)) (name)) (fieldMap)) (cx))) (fun (field_field : Name) => (inr) ((Build_Projection) (field_typeName) (field_field))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("typeName"%string)) (name)) (fieldMap)) (cx))) (fun (field_typeName : Name) => ((eithers.bind) (((((requireField) ("fields"%string)) ((decodeList) (field))) (fieldMap)) (cx))) (fun (field_fields : (list) (Field)) => (inr) ((Build_Record_) (field_typeName) (field_fields))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("annotated"%string) (fun (input : Term) => ((eithers.map) (fun (t : AnnotatedTerm) => (Term_Annotated) (t))) (((annotatedTerm) (cx)) (input)))) ((cons) ((pair) ("application"%string) (fun (input : Term) => ((eithers.map) (fun (t : Application) => (Term_Application) (t))) (((application) (cx)) (input)))) ((cons) ((pair) ("cases"%string) (fun (input : Term) => ((eithers.map) (fun (t : CaseStatement) => (Term_Cases) (t))) (((caseStatement) (cx)) (input)))) ((cons) ((pair) ("either"%string) (fun (input : Term) => ((eithers.map) (fun (t : (sum) (Term) (Term)) => (Term_Either) (t))) (((((decodeEither) (term)) (term)) (cx)) (input)))) ((cons) ((pair) ("inject"%string) (fun (input : Term) => ((eithers.map) (fun (t : Injection) => (Term_Inject) (t))) (((injection) (cx)) (input)))) ((cons) ((pair) ("lambda"%string) (fun (input : Term) => ((eithers.map) (fun (t : Lambda) => (Term_Lambda) (t))) (((lambda) (cx)) (input)))) ((cons) ((pair) ("let"%string) (fun (input : Term) => ((eithers.map) (fun (t : Let) => (Term_Let) (t))) (((let_) (cx)) (input)))) ((cons) ((pair) ("list"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) (Term)) => (Term_List) (t))) ((((decodeList) (term)) (cx)) (input)))) ((cons) ((pair) ("literal"%string) (fun (input : Term) => ((eithers.map) (fun (t : Literal) => (Term_Literal) (t))) (((literal) (cx)) (input)))) ((cons) ((pair) ("map"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) ((prod) (Term) (Term))) => (Term_Map) (t))) (((((decodeMap) (term)) (term)) (cx)) (input)))) ((cons) ((pair) ("maybe"%string) (fun (input : Term) => ((eithers.map) (fun (t : (option) (Term)) => (Term_Maybe) (t))) ((((decodeMaybe) (term)) (cx)) (input)))) ((cons) ((pair) ("pair"%string) (fun (input : Term) => ((eithers.map) (fun (t : (prod) (Term) (Term)) => (Term_Pair) (t))) (((((decodePair) (term)) (term)) (cx)) (input)))) ((cons) ((pair) ("project"%string) (fun (input : Term) => ((eithers.map) (fun (t : Projection) => (Term_Project) (t))) (((projection) (cx)) (input)))) ((cons) ((pair) ("record"%string) (fun (input : Term) => ((eithers.map) (fun (t : Record_) => (Term_Record) (t))) (((record) (cx)) (input)))) ((cons) ((pair) ("set"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) (Term)) => (Term_Set) (t))) ((((decodeSet) (term)) (cx)) (input)))) ((cons) ((pair) ("typeApplication"%string) (fun (input : Term) => ((eithers.map) (fun (t : TypeApplicationTerm) => (Term_TypeApplication) (t))) (((typeApplicationTerm) (cx)) (input)))) ((cons) ((pair) ("typeLambda"%string) (fun (input : Term) => ((eithers.map) (fun (t : TypeLambda) => (Term_TypeLambda) (t))) (((typeLambda) (cx)) (input)))) ((cons) ((pair) ("unit"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Term_Unit) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("unwrap"%string) (fun (input : Term) => ((eithers.map) (fun (t : Name) => (Term_Unwrap) (t))) (((name) (cx)) (input)))) ((cons) ((pair) ("variable"%string) (fun (input : Term) => ((eithers.map) (fun (t : Name) => (Term_Variable) (t))) (((name) (cx)) (input)))) ((cons) ((pair) ("wrap"%string) (fun (input : Term) => ((eithers.map) (fun (t : WrappedTerm) => (Term_Wrap) (t))) (((wrappedTerm) (cx)) (input)))) (nil)))))))))))))))))))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Term)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("annotated"%string) (fun (input : Term) => ((eithers.map) (fun (t : AnnotatedType) => (Type__Annotated) (t))) (((annotatedType) (cx)) (input)))) ((cons) ((pair) ("application"%string) (fun (input : Term) => ((eithers.map) (fun (t : ApplicationType) => (Type__Application) (t))) (((applicationType) (cx)) (input)))) ((cons) ((pair) ("either"%string) (fun (input : Term) => ((eithers.map) (fun (t : EitherType) => (Type__Either) (t))) (((eitherType) (cx)) (input)))) ((cons) ((pair) ("forall"%string) (fun (input : Term) => ((eithers.map) (fun (t : ForallType) => (Type__Forall) (t))) (((forallType) (cx)) (input)))) ((cons) ((pair) ("function"%string) (fun (input : Term) => ((eithers.map) (fun (t : FunctionType) => (Type__Function) (t))) (((functionType) (cx)) (input)))) ((cons) ((pair) ("list"%string) (fun (input : Term) => ((eithers.map) (fun (t : Type_) => (Type__List) (t))) (((type) (cx)) (input)))) ((cons) ((pair) ("literal"%string) (fun (input : Term) => ((eithers.map) (fun (t : LiteralType) => (Type__Literal) (t))) (((literalType) (cx)) (input)))) ((cons) ((pair) ("map"%string) (fun (input : Term) => ((eithers.map) (fun (t : MapType) => (Type__Map) (t))) (((mapType) (cx)) (input)))) ((cons) ((pair) ("maybe"%string) (fun (input : Term) => ((eithers.map) (fun (t : Type_) => (Type__Maybe) (t))) (((type) (cx)) (input)))) ((cons) ((pair) ("pair"%string) (fun (input : Term) => ((eithers.map) (fun (t : PairType) => (Type__Pair) (t))) (((pairType) (cx)) (input)))) ((cons) ((pair) ("record"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) (FieldType)) => (Type__Record) (t))) ((((decodeList) (fieldType)) (cx)) (input)))) ((cons) ((pair) ("set"%string) (fun (input : Term) => ((eithers.map) (fun (t : Type_) => (Type__Set) (t))) (((type) (cx)) (input)))) ((cons) ((pair) ("union"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) (FieldType)) => (Type__Union) (t))) ((((decodeList) (fieldType)) (cx)) (input)))) ((cons) ((pair) ("unit"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Type__Unit) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("variable"%string) (fun (input : Term) => ((eithers.map) (fun (t : Name) => (Type__Variable) (t))) (((name) (cx)) (input)))) ((cons) ((pair) ("void"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Type__Void) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("wrap"%string) (fun (input : Term) => ((eithers.map) (fun (t : Type_) => (Type__Wrap) (t))) (((type) (cx)) (input)))) (nil)))))))))))))))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Type_)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("body"%string)) (term)) (fieldMap)) (cx))) (fun (field_body : Term) => ((eithers.bind) (((((requireField) ("type"%string)) (type)) (fieldMap)) (cx))) (fun (field_type : Type_) => (inr) ((Build_TypeApplicationTerm) (field_body) (field_type))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("parameter"%string)) (name)) (fieldMap)) (cx))) (fun (field_parameter : Name) => ((eithers.bind) (((((requireField) ("body"%string)) (term)) (fieldMap)) (cx))) (fun (field_body : Term) => (inr) ((Build_TypeLambda) (field_parameter) (field_body))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("variables"%string)) ((decodeList) (name))) (fieldMap)) (cx))) (fun (field_variables : (list) (Name)) => ((eithers.bind) (((((requireField) ("type"%string)) (type)) (fieldMap)) (cx))) (fun (field_type : Type_) => ((eithers.bind) (((((requireField) ("constraints"%string)) ((decodeMaybe) (((decodeMap) (name)) (typeVariableMetadata)))) (fieldMap)) (cx))) (fun (field_constraints : (option) ((list) ((prod) (Name) (TypeVariableMetadata)))) => (inr) ((Build_TypeScheme) (field_variables) (field_type) (field_constraints)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("classes"%string)) ((decodeSet) (name))) (fieldMap)) (cx))) (fun (field_classes : (list) (Name)) => (inr) ((Build_TypeVariableMetadata) (field_classes)))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("typeName"%string)) (name)) (fieldMap)) (cx))) (fun (field_typeName : Name) => ((eithers.bind) (((((requireField) ("body"%string)) (term)) (fieldMap)) (cx))) (fun (field_body : Term) => (inr) ((Build_WrappedTerm) (field_typeName) (field_body))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))).

Definition annotatedTerm : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (AnnotatedTerm) :=
  (fst annotatedTerm_annotatedType_bundle).
Definition annotatedType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (AnnotatedType) :=
  (fst (snd annotatedTerm_annotatedType_bundle)).
Definition application : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Application) :=
  (fst (snd (snd annotatedTerm_annotatedType_bundle))).
Definition applicationType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ApplicationType) :=
  (fst (snd (snd (snd annotatedTerm_annotatedType_bundle)))).
Definition binding : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Binding) :=
  (fst (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))).
Definition caseStatement : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (CaseStatement) :=
  (fst (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))).
Definition eitherType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (EitherType) :=
  (fst (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))).
Definition field : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Field) :=
  (fst (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))).
Definition fieldType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FieldType) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))).
Definition floatType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FloatType) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))).
Definition floatValue : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FloatValue) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))).
Definition forallType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ForallType) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))).
Definition functionType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FunctionType) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))).
Definition injection : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Injection) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))).
Definition integerType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (IntegerType) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))).
Definition integerValue : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (IntegerValue) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))).
Definition lambda : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Lambda) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))).
Definition let_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Let) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))).
Definition literal : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Literal) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))).
Definition literalType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (LiteralType) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))).
Definition mapType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (MapType) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))))).
Definition name : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Name) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))))).
Definition pairType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (PairType) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))))))).
Definition projection : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Projection) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))))))).
Definition record : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Record_) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))))))))).
Definition term : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Term) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))))))))).
Definition type : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Type_) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))))))))))).
Definition typeApplicationTerm : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeApplicationTerm) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))))))))))).
Definition typeLambda : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeLambda) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))))))))))))).
Definition typeScheme : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeScheme) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))))))))))))).
Definition typeVariableMetadata : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeVariableMetadata) :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))))))))))))))).
Definition wrappedTerm : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (WrappedTerm) :=
  (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))))))))))))))).

