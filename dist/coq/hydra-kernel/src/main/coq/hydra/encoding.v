(* Functions for generating term encoders from type modules *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.annotations hydra.constants hydra.context hydra.core hydra.decode.core hydra.encode.core hydra.errors hydra.formatting hydra.graph hydra.lib.eithers hydra.lib.lists hydra.lib.logic hydra.lib.maps hydra.lib.maybes hydra.lib.sets hydra.lib.strings hydra.names hydra.packaging hydra.predicates.

Definition encodeBindingName : forall (_ : Name) , Name := fun (n : Name) => (((logic.ifElse) ((logic.not) ((lists.null) ((lists.tail) (((strings.splitOn) ("."%string)) ((fun w_ => w_) (n))))))) (((strings.intercalate) ("."%string)) (((lists.concat2) ((cons) ("hydra"%string) ((cons) ("encode"%string) (nil)))) (((lists.concat2) ((lists.tail) ((lists.init) (((strings.splitOn) ("."%string)) ((fun w_ => w_) (n)))))) ((cons) ((decapitalize) ((localNameOf) (n))) (nil)))))) ((decapitalize) ((localNameOf) (n))).
Definition encodeFloatValue : forall (_ : FloatType) , forall (_ : Term) , Term := fun (floatType : FloatType) => fun (valTerm : Term) => (Term_Inject) ((Build_Injection) ("hydra.core.FloatValue"%string) ((Build_Field) ((fun x_ => match x_ with
| FloatType_Bigfloat _ => "bigfloat"%string
| FloatType_Float32 _ => "float32"%string
| FloatType_Float64 _ => "float64"%string
end) (floatType)) (valTerm))).
Definition encodeIntegerValue : forall (_ : IntegerType) , forall (_ : Term) , Term := fun (intType : IntegerType) => fun (valTerm : Term) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ((fun x_ => match x_ with
| IntegerType_Bigint _ => "bigint"%string
| IntegerType_Int8 _ => "int8"%string
| IntegerType_Int16 _ => "int16"%string
| IntegerType_Int32 _ => "int32"%string
| IntegerType_Int64 _ => "int64"%string
| IntegerType_Uint8 _ => "uint8"%string
| IntegerType_Uint16 _ => "uint16"%string
| IntegerType_Uint32 _ => "uint32"%string
| IntegerType_Uint64 _ => "uint64"%string
end) (intType)) (valTerm))).
Definition encodeLiteralType : forall (_ : LiteralType) , Term := fun x_ => match x_ with
| LiteralType_Binary _ => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("literal"%string) ((Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("binary"%string) ((Term_Variable) ("x"%string)))))))))
| LiteralType_Boolean _ => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("literal"%string) ((Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("boolean"%string) ((Term_Variable) ("x"%string)))))))))
| LiteralType_Decimal _ => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("literal"%string) ((Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("decimal"%string) ((Term_Variable) ("x"%string)))))))))
| LiteralType_String _ => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("literal"%string) ((Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("string"%string) ((Term_Variable) ("x"%string)))))))))
| LiteralType_Integer v_ => (fun (intType : IntegerType) => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("literal"%string) ((Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("integer"%string) (((encodeIntegerValue) (intType)) ((Term_Variable) ("x"%string))))))))))) (v_)
| LiteralType_Float v_ => (fun (floatType : FloatType) => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("literal"%string) ((Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("float"%string) (((encodeFloatValue) (floatType)) ((Term_Variable) ("x"%string))))))))))) (v_)
end.
Definition encodeName : forall (_ : Name) , Term := fun (n : Name) => (Term_Wrap) ((Build_WrappedTerm) ("hydra.core.Name"%string) ((Term_Literal) ((Literal_String) ((fun w_ => w_) (n))))).
Definition encodeInjection : forall (_ : Name) , forall (_ : Name) , forall (_ : Term) , Term := fun (typeName : Name) => fun (fieldName : Name) => fun (fieldTerm : Term) => (Term_Record) ((Build_Record_) ("hydra.core.Injection"%string) ((cons) ((Build_Field) ("typeName"%string) ((encodeName) (typeName))) ((cons) ((Build_Field) ("field"%string) (((fun (fname : Name) => fun (fterm : Term) => (Term_Record) ((Build_Record_) ("hydra.core.Field"%string) ((cons) ((Build_Field) ("name"%string) ((encodeName) (fname))) ((cons) ((Build_Field) ("term"%string) (fterm)) (nil))))) (fieldName)) (fieldTerm))) (nil)))).
Definition encodeEitherType_encodeFieldValue_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : EitherType) , Term) (prod (forall (_ : Name) , forall (_ : Name) , forall (_ : Type_) , Term) (prod (forall (_ : ForallType) , Term) (prod (forall (_ : Type_) , Term) (prod (forall (_ : MapType) , Term) (prod (forall (_ : Type_) , Term) (prod (forall (_ : PairType) , Term) (prod (forall (_ : (list) (FieldType)) , Term) (prod (forall (_ : Name) , forall (_ : (list) (FieldType)) , Term) (prod (forall (_ : Type_) , Term) (prod (forall (_ : Type_) , Term) (prod (forall (_ : (list) (FieldType)) , Term) (prod (forall (_ : Name) , forall (_ : (list) (FieldType)) , Term) (prod (forall (_ : Type_) , Term) (forall (_ : Name) , forall (_ : Type_) , Term))))))))))))))) =>
    let encodeEitherType := (fst bundle_) in
    let encodeFieldValue := (fst (snd bundle_)) in
    let encodeForallType := (fst (snd (snd bundle_))) in
    let encodeListType := (fst (snd (snd (snd bundle_)))) in
    let encodeMapType := (fst (snd (snd (snd (snd bundle_))))) in
    let encodeOptionalType := (fst (snd (snd (snd (snd (snd bundle_)))))) in
    let encodePairType := (fst (snd (snd (snd (snd (snd (snd bundle_))))))) in
    let encodeRecordType := (fst (snd (snd (snd (snd (snd (snd (snd bundle_)))))))) in
    let encodeRecordTypeNamed := (fst (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))) in
    let encodeSetType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))) in
    let encodeType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))) in
    let encodeUnionType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))) in
    let encodeUnionTypeNamed := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))) in
    let encodeWrappedType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))) in
    let encodeWrappedTypeNamed := (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))) in
    (pair (fun (et : EitherType) => (Term_Lambda) ((Build_Lambda) ("e"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("either"%string) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Variable) ("hydra.lib.eithers.bimap"%string)) ((encodeType) ((fun r_ => (eitherType_left) (r_)) (et))))) ((encodeType) ((fun r_ => (eitherType_right) (r_)) (et))))) ((Term_Variable) ("e"%string))))))))) ((pair (fun (typeName : Name) => fun (fieldName : Name) => fun (fieldType : Type_) => (Term_Lambda) ((Build_Lambda) ("y"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("inject"%string) ((((encodeInjection) (typeName)) (fieldName)) ((Term_Application) ((Build_Application) ((encodeType) (fieldType)) ((Term_Variable) ("y"%string)))))))))) ((pair (fun (ft : ForallType) => (Term_Lambda) ((Build_Lambda) ((encodeBindingName) ((fun r_ => (forallType_parameter) (r_)) (ft))) ((None) : (option) (Type_)) ((encodeType) ((fun r_ => (forallType_body) (r_)) (ft))))) ((pair (fun (elemType : Type_) => (Term_Lambda) ((Build_Lambda) ("xs"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("list"%string) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Variable) ("hydra.lib.lists.map"%string)) ((encodeType) (elemType)))) ((Term_Variable) ("xs"%string))))))))) ((pair (fun (mt : MapType) => (Term_Lambda) ((Build_Lambda) ("m"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("map"%string) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Variable) ("hydra.lib.maps.bimap"%string)) ((encodeType) ((fun r_ => (mapType_keys) (r_)) (mt))))) ((encodeType) ((fun r_ => (mapType_values) (r_)) (mt))))) ((Term_Variable) ("m"%string))))))))) ((pair (fun (elemType : Type_) => (Term_Lambda) ((Build_Lambda) ("opt"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("maybe"%string) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Variable) ("hydra.lib.maybes.map"%string)) ((encodeType) (elemType)))) ((Term_Variable) ("opt"%string))))))))) ((pair (fun (pt : PairType) => (Term_Lambda) ((Build_Lambda) ("p"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("pair"%string) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Variable) ("hydra.lib.pairs.bimap"%string)) ((encodeType) ((fun r_ => (pairType_first) (r_)) (pt))))) ((encodeType) ((fun r_ => (pairType_second) (r_)) (pt))))) ((Term_Variable) ("p"%string))))))))) ((pair (fun (rt : (list) (FieldType)) => ((encodeRecordTypeNamed) ("unknown"%string)) (rt)) ((pair (fun (ename : Name) => fun (rt : (list) (FieldType)) => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("record"%string) ((Term_Record) ((Build_Record_) ("hydra.core.Record"%string) ((cons) ((Build_Field) ("typeName"%string) ((encodeName) (ename))) ((cons) ((Build_Field) ("fields"%string) ((Term_List) (((lists.map) (((fun (tname : Name) => fun (recType : (list) (FieldType)) => fun (ft : FieldType) => (Term_Record) ((Build_Record_) ("hydra.core.Field"%string) ((cons) ((Build_Field) ("name"%string) ((encodeName) ((fun r_ => (fieldType_name) (r_)) (ft)))) ((cons) ((Build_Field) ("term"%string) ((Term_Application) ((Build_Application) ((encodeType) ((fun r_ => (fieldType_type) (r_)) (ft))) ((Term_Application) ((Build_Application) ((Term_Project) ((Build_Projection) (tname) ((fun r_ => (fieldType_name) (r_)) (ft)))) ((Term_Variable) ("x"%string))))))) (nil))))) (ename)) (rt))) (rt)))) (nil)))))))))) ((pair (fun (elemType : Type_) => (Term_Lambda) ((Build_Lambda) ("s"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("set"%string) ((Term_Application) ((Build_Application) ((Term_Application) ((Build_Application) ((Term_Variable) ("hydra.lib.sets.map"%string)) ((encodeType) (elemType)))) ((Term_Variable) ("s"%string))))))))) ((pair (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (encodeType) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Application v_ => (fun (appType : ApplicationType) => (Term_Application) ((Build_Application) ((encodeType) ((fun r_ => (applicationType_function) (r_)) (appType))) ((encodeType) ((fun r_ => (applicationType_argument) (r_)) (appType))))) (v_)
| Type__Either v_ => (fun (et : EitherType) => (encodeEitherType) (et)) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (encodeForallType) (ft)) (v_)
| Type__Function v_ => (fun (_ : FunctionType) => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Variable) ("x"%string)))) (v_)
| Type__List v_ => (fun (elemType : Type_) => (encodeListType) (elemType)) (v_)
| Type__Literal v_ => (fun (lt : LiteralType) => (encodeLiteralType) (lt)) (v_)
| Type__Map v_ => (fun (mt : MapType) => (encodeMapType) (mt)) (v_)
| Type__Maybe v_ => (fun (elemType : Type_) => (encodeOptionalType) (elemType)) (v_)
| Type__Pair v_ => (fun (pt : PairType) => (encodePairType) (pt)) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => (encodeRecordType) (rt)) (v_)
| Type__Set v_ => (fun (elemType : Type_) => (encodeSetType) (elemType)) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => (encodeUnionType) (rt)) (v_)
| Type__Wrap v_ => (fun (wt : Type_) => (encodeWrappedType) (wt)) (v_)
| Type__Unit _ => (Term_Lambda) ((Build_Lambda) ("_"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("unit"%string) ((Term_Unit) (tt))))))
| Type__Void _ => (Term_Lambda) ((Build_Lambda) ("_"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("unit"%string) ((Term_Unit) (tt))))))
| Type__Variable v_ => (fun (typeName : Name) => (Term_Variable) ((encodeBindingName) (typeName))) (v_)
end) ((pair (fun (rt : (list) (FieldType)) => ((encodeUnionTypeNamed) ("unknown"%string)) (rt)) ((pair (fun (ename : Name) => fun (rt : (list) (FieldType)) => (Term_Cases) ((Build_CaseStatement) (ename) ((None) : (option) (Term)) (((lists.map) (fun (ft : FieldType) => (Build_Field) ((fun r_ => (fieldType_name) (r_)) (ft)) ((((encodeFieldValue) (ename)) ((fun r_ => (fieldType_name) (r_)) (ft))) ((fun r_ => (fieldType_type) (r_)) (ft))))) (rt)))) ((pair (fun (wt : Type_) => ((encodeWrappedTypeNamed) ("unknown"%string)) (wt)) (fun (ename : Name) => fun (wt : Type_) => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("wrap"%string) ((Term_Record) ((Build_Record_) ("hydra.core.WrappedTerm"%string) ((cons) ((Build_Field) ("typeName"%string) ((encodeName) (ename))) ((cons) ((Build_Field) ("body"%string) ((Term_Application) ((Build_Application) ((encodeType) (wt)) ((Term_Application) ((Build_Application) ((Term_Unwrap) (ename)) ((Term_Variable) ("x"%string))))))) (nil)))))))))))))))))))))))))))))))))))))).

Definition encodeEitherType : forall (_ : EitherType) , Term :=
  (fst encodeEitherType_encodeFieldValue_bundle).
Definition encodeFieldValue : forall (_ : Name) , forall (_ : Name) , forall (_ : Type_) , Term :=
  (fst (snd encodeEitherType_encodeFieldValue_bundle)).
Definition encodeForallType : forall (_ : ForallType) , Term :=
  (fst (snd (snd encodeEitherType_encodeFieldValue_bundle))).
Definition encodeListType : forall (_ : Type_) , Term :=
  (fst (snd (snd (snd encodeEitherType_encodeFieldValue_bundle)))).
Definition encodeMapType : forall (_ : MapType) , Term :=
  (fst (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle))))).
Definition encodeOptionalType : forall (_ : Type_) , Term :=
  (fst (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle)))))).
Definition encodePairType : forall (_ : PairType) , Term :=
  (fst (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle))))))).
Definition encodeRecordType : forall (_ : (list) (FieldType)) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle)))))))).
Definition encodeRecordTypeNamed : forall (_ : Name) , forall (_ : (list) (FieldType)) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle))))))))).
Definition encodeSetType : forall (_ : Type_) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle)))))))))).
Definition encodeType : forall (_ : Type_) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle))))))))))).
Definition encodeUnionType : forall (_ : (list) (FieldType)) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle)))))))))))).
Definition encodeUnionTypeNamed : forall (_ : Name) , forall (_ : (list) (FieldType)) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle))))))))))))).
Definition encodeWrappedType : forall (_ : Type_) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle)))))))))))))).
Definition encodeWrappedTypeNamed : forall (_ : Name) , forall (_ : Type_) , Term :=
  (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd encodeEitherType_encodeFieldValue_bundle)))))))))))))).
Definition encodeTypeNamed_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Name) , forall (_ : Type_) , Term) =>
    let encodeTypeNamed := bundle_ in
    fun (ename : Name) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => ((encodeTypeNamed) (ename)) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Application v_ => (fun (appType : ApplicationType) => (Term_Application) ((Build_Application) ((encodeType) ((fun r_ => (applicationType_function) (r_)) (appType))) ((encodeType) ((fun r_ => (applicationType_argument) (r_)) (appType))))) (v_)
| Type__Either v_ => (fun (et : EitherType) => (encodeEitherType) (et)) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (Term_Lambda) ((Build_Lambda) ((encodeBindingName) ((fun r_ => (forallType_parameter) (r_)) (ft))) ((None) : (option) (Type_)) (((encodeTypeNamed) (ename)) ((fun r_ => (forallType_body) (r_)) (ft))))) (v_)
| Type__Function v_ => (fun (_ : FunctionType) => (Term_Lambda) ((Build_Lambda) ("x"%string) ((None) : (option) (Type_)) ((Term_Variable) ("x"%string)))) (v_)
| Type__List v_ => (fun (elemType : Type_) => (encodeListType) (elemType)) (v_)
| Type__Literal v_ => (fun (lt : LiteralType) => (encodeLiteralType) (lt)) (v_)
| Type__Map v_ => (fun (mt : MapType) => (encodeMapType) (mt)) (v_)
| Type__Maybe v_ => (fun (elemType : Type_) => (encodeOptionalType) (elemType)) (v_)
| Type__Pair v_ => (fun (pt : PairType) => (encodePairType) (pt)) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => ((encodeRecordTypeNamed) (ename)) (rt)) (v_)
| Type__Set v_ => (fun (elemType : Type_) => (encodeSetType) (elemType)) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => ((encodeUnionTypeNamed) (ename)) (rt)) (v_)
| Type__Wrap v_ => (fun (wt : Type_) => ((encodeWrappedTypeNamed) (ename)) (wt)) (v_)
| Type__Unit _ => (Term_Lambda) ((Build_Lambda) ("_"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("unit"%string) ((Term_Unit) (tt))))))
| Type__Void _ => (Term_Lambda) ((Build_Lambda) ("_"%string) ((None) : (option) (Type_)) ((Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("unit"%string) ((Term_Unit) (tt))))))
| Type__Variable v_ => (fun (typeName : Name) => (Term_Variable) ((encodeBindingName) (typeName))) (v_)
end) (typ)).

Definition encodeTypeNamed : forall (_ : Name) , forall (_ : Type_) , Term :=
  encodeTypeNamed_bundle.
Definition encoderCollectForallVariables_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , (list) (Name)) =>
    let encoderCollectForallVariables := bundle_ in
    fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (encoderCollectForallVariables) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => ((lists.cons) ((fun r_ => (forallType_parameter) (r_)) (ft))) ((encoderCollectForallVariables) ((fun r_ => (forallType_body) (r_)) (ft)))) (v_)
| _ => nil
end) (typ)).

Definition encoderCollectForallVariables : forall (_ : Type_) , (list) (Name) :=
  encoderCollectForallVariables_bundle.
Definition encoderCollectTypeVarsFromType_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , (list) (Name)) =>
    let encoderCollectTypeVarsFromType := bundle_ in
    fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (encoderCollectTypeVarsFromType) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Application v_ => (fun (appType : ApplicationType) => ((lists.concat2) ((encoderCollectTypeVarsFromType) ((fun r_ => (applicationType_function) (r_)) (appType)))) ((encoderCollectTypeVarsFromType) ((fun r_ => (applicationType_argument) (r_)) (appType)))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (encoderCollectTypeVarsFromType) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| Type__List v_ => (fun (elemType : Type_) => (encoderCollectTypeVarsFromType) (elemType)) (v_)
| Type__Map v_ => (fun (mt : MapType) => ((lists.concat2) ((encoderCollectTypeVarsFromType) ((fun r_ => (mapType_keys) (r_)) (mt)))) ((encoderCollectTypeVarsFromType) ((fun r_ => (mapType_values) (r_)) (mt)))) (v_)
| Type__Maybe v_ => (fun (elemType : Type_) => (encoderCollectTypeVarsFromType) (elemType)) (v_)
| Type__Pair v_ => (fun (pt : PairType) => ((lists.concat2) ((encoderCollectTypeVarsFromType) ((fun r_ => (pairType_first) (r_)) (pt)))) ((encoderCollectTypeVarsFromType) ((fun r_ => (pairType_second) (r_)) (pt)))) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => (lists.concat) (((lists.map) (fun (ft : FieldType) => (encoderCollectTypeVarsFromType) ((fun r_ => (fieldType_type) (r_)) (ft)))) (rt))) (v_)
| Type__Set v_ => (fun (elemType : Type_) => (encoderCollectTypeVarsFromType) (elemType)) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => (lists.concat) (((lists.map) (fun (ft : FieldType) => (encoderCollectTypeVarsFromType) ((fun r_ => (fieldType_type) (r_)) (ft)))) (rt))) (v_)
| Type__Variable v_ => (fun (name : Name) => (cons) (name) (nil)) (v_)
| Type__Wrap v_ => (fun (wt : Type_) => (encoderCollectTypeVarsFromType) (wt)) (v_)
| _ => nil
end) (typ)).

Definition encoderCollectTypeVarsFromType : forall (_ : Type_) , (list) (Name) :=
  encoderCollectTypeVarsFromType_bundle.
Definition encoderCollectOrdVars_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , (list) (Name)) =>
    let encoderCollectOrdVars := bundle_ in
    fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (encoderCollectOrdVars) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Application v_ => (fun (appType : ApplicationType) => ((lists.concat2) ((encoderCollectOrdVars) ((fun r_ => (applicationType_function) (r_)) (appType)))) ((encoderCollectOrdVars) ((fun r_ => (applicationType_argument) (r_)) (appType)))) (v_)
| Type__Either v_ => (fun (et : EitherType) => ((lists.concat2) ((encoderCollectOrdVars) ((fun r_ => (eitherType_left) (r_)) (et)))) ((encoderCollectOrdVars) ((fun r_ => (eitherType_right) (r_)) (et)))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (encoderCollectOrdVars) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| Type__List v_ => (fun (elemType : Type_) => (encoderCollectOrdVars) (elemType)) (v_)
| Type__Map v_ => (fun (mt : MapType) => (lists.concat) ((cons) ((encoderCollectTypeVarsFromType) ((fun r_ => (mapType_keys) (r_)) (mt))) ((cons) ((encoderCollectOrdVars) ((fun r_ => (mapType_keys) (r_)) (mt))) ((cons) ((encoderCollectOrdVars) ((fun r_ => (mapType_values) (r_)) (mt))) (nil))))) (v_)
| Type__Maybe v_ => (fun (elemType : Type_) => (encoderCollectOrdVars) (elemType)) (v_)
| Type__Pair v_ => (fun (pt : PairType) => ((lists.concat2) ((encoderCollectOrdVars) ((fun r_ => (pairType_first) (r_)) (pt)))) ((encoderCollectOrdVars) ((fun r_ => (pairType_second) (r_)) (pt)))) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => (lists.concat) (((lists.map) (fun (ft : FieldType) => (encoderCollectOrdVars) ((fun r_ => (fieldType_type) (r_)) (ft)))) (rt))) (v_)
| Type__Set v_ => (fun (elemType : Type_) => ((lists.concat2) ((encoderCollectTypeVarsFromType) (elemType))) ((encoderCollectOrdVars) (elemType))) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => (lists.concat) (((lists.map) (fun (ft : FieldType) => (encoderCollectOrdVars) ((fun r_ => (fieldType_type) (r_)) (ft)))) (rt))) (v_)
| Type__Wrap v_ => (fun (wt : Type_) => (encoderCollectOrdVars) (wt)) (v_)
| _ => nil
end) (typ)).

Definition encoderCollectOrdVars : forall (_ : Type_) , (list) (Name) :=
  encoderCollectOrdVars_bundle.
Definition encoderFullResultType_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , Type_) =>
    let encoderFullResultType := bundle_ in
    fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (encoderFullResultType) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Application v_ => (fun (appType : ApplicationType) => (Type__Application) ((Build_ApplicationType) ((encoderFullResultType) ((fun r_ => (applicationType_function) (r_)) (appType))) ((fun r_ => (applicationType_argument) (r_)) (appType)))) (v_)
| Type__Either v_ => (fun (et : EitherType) => (Type__Either) ((Build_EitherType) ((encoderFullResultType) ((fun r_ => (eitherType_left) (r_)) (et))) ((encoderFullResultType) ((fun r_ => (eitherType_right) (r_)) (et))))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (Type__Application) ((Build_ApplicationType) ((encoderFullResultType) ((fun r_ => (forallType_body) (r_)) (ft))) ((Type__Variable) ((fun r_ => (forallType_parameter) (r_)) (ft))))) (v_)
| Type__List v_ => (fun (elemType : Type_) => (Type__List) ((encoderFullResultType) (elemType))) (v_)
| Type__Literal v_ => (fun (_ : LiteralType) => (Type__Variable) ("hydra.core.Literal"%string)) (v_)
| Type__Map v_ => (fun (mt : MapType) => (Type__Map) ((Build_MapType) ((encoderFullResultType) ((fun r_ => (mapType_keys) (r_)) (mt))) ((encoderFullResultType) ((fun r_ => (mapType_values) (r_)) (mt))))) (v_)
| Type__Maybe v_ => (fun (elemType : Type_) => (Type__Maybe) ((encoderFullResultType) (elemType))) (v_)
| Type__Pair v_ => (fun (pt : PairType) => (Type__Pair) ((Build_PairType) ((encoderFullResultType) ((fun r_ => (pairType_first) (r_)) (pt))) ((encoderFullResultType) ((fun r_ => (pairType_second) (r_)) (pt))))) (v_)
| Type__Record v_ => (fun (_ : (list) (FieldType)) => (Type__Variable) ("hydra.core.Term"%string)) (v_)
| Type__Set v_ => (fun (elemType : Type_) => (Type__Set) ((encoderFullResultType) (elemType))) (v_)
| Type__Union v_ => (fun (_ : (list) (FieldType)) => (Type__Variable) ("hydra.core.Term"%string)) (v_)
| Type__Unit _ => (Type__Unit) (tt)
| Type__Variable v_ => (fun (name : Name) => (Type__Variable) (name)) (v_)
| Type__Void _ => (Type__Void) (tt)
| Type__Wrap v_ => (fun (_ : Type_) => (Type__Variable) ("hydra.core.Term"%string)) (v_)
| _ => (Type__Variable) ("hydra.core.Term"%string)
end) (typ)).

Definition encoderFullResultType : forall (_ : Type_) , Type_ :=
  encoderFullResultType_bundle.
Definition encoderFullResultTypeNamed_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Name) , forall (_ : Type_) , Type_) =>
    let encoderFullResultTypeNamed := bundle_ in
    fun (ename : Name) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => ((encoderFullResultTypeNamed) (ename)) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Application v_ => (fun (appType : ApplicationType) => (Type__Application) ((Build_ApplicationType) ((encoderFullResultType) ((fun r_ => (applicationType_function) (r_)) (appType))) ((fun r_ => (applicationType_argument) (r_)) (appType)))) (v_)
| Type__Either v_ => (fun (et : EitherType) => (Type__Either) ((Build_EitherType) ((encoderFullResultType) ((fun r_ => (eitherType_left) (r_)) (et))) ((encoderFullResultType) ((fun r_ => (eitherType_right) (r_)) (et))))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (Type__Application) ((Build_ApplicationType) (((encoderFullResultTypeNamed) (ename)) ((fun r_ => (forallType_body) (r_)) (ft))) ((Type__Variable) ((fun r_ => (forallType_parameter) (r_)) (ft))))) (v_)
| Type__List v_ => (fun (elemType : Type_) => (Type__List) ((encoderFullResultType) (elemType))) (v_)
| Type__Literal v_ => (fun (_ : LiteralType) => (Type__Variable) ("hydra.core.Literal"%string)) (v_)
| Type__Map v_ => (fun (mt : MapType) => (Type__Map) ((Build_MapType) ((encoderFullResultType) ((fun r_ => (mapType_keys) (r_)) (mt))) ((encoderFullResultType) ((fun r_ => (mapType_values) (r_)) (mt))))) (v_)
| Type__Maybe v_ => (fun (elemType : Type_) => (Type__Maybe) ((encoderFullResultType) (elemType))) (v_)
| Type__Pair v_ => (fun (pt : PairType) => (Type__Pair) ((Build_PairType) ((encoderFullResultType) ((fun r_ => (pairType_first) (r_)) (pt))) ((encoderFullResultType) ((fun r_ => (pairType_second) (r_)) (pt))))) (v_)
| Type__Record v_ => (fun (_ : (list) (FieldType)) => (Type__Variable) (ename)) (v_)
| Type__Set v_ => (fun (elemType : Type_) => (Type__Set) ((encoderFullResultType) (elemType))) (v_)
| Type__Union v_ => (fun (_ : (list) (FieldType)) => (Type__Variable) (ename)) (v_)
| Type__Unit _ => (Type__Unit) (tt)
| Type__Variable v_ => (fun (name : Name) => (Type__Variable) (name)) (v_)
| Type__Void _ => (Type__Void) (tt)
| Type__Wrap v_ => (fun (_ : Type_) => (Type__Variable) (ename)) (v_)
| _ => (Type__Variable) ("hydra.core.Term"%string)
end) (typ)).

Definition encoderFullResultTypeNamed : forall (_ : Name) , forall (_ : Type_) , Type_ :=
  encoderFullResultTypeNamed_bundle.
Definition prependForallEncoders_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , forall (_ : Type_) , Type_) =>
    let prependForallEncoders := bundle_ in
    fun (baseType : Type_) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => ((prependForallEncoders) (baseType)) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (Type__Function) ((Build_FunctionType) ((Type__Function) ((Build_FunctionType) ((Type__Variable) ((fun r_ => (forallType_parameter) (r_)) (ft))) ((Type__Variable) ("hydra.core.Term"%string)))) (((prependForallEncoders) (baseType)) ((fun r_ => (forallType_body) (r_)) (ft))))) (v_)
| _ => baseType
end) (typ)).

Definition prependForallEncoders : forall (_ : Type_) , forall (_ : Type_) , Type_ :=
  prependForallEncoders_bundle.
Definition encoderTypeNamed : forall (_ : Name) , forall (_ : Type_) , Type_ := fun (ename : Name) => fun (typ : Type_) => let resultType := ((encoderFullResultTypeNamed) (ename)) (typ) in let baseType := (Type__Function) ((Build_FunctionType) (resultType) ((Type__Variable) ("hydra.core.Term"%string))) in ((prependForallEncoders) (baseType)) (typ).
Definition encoderTypeSchemeNamed : forall (_ : Name) , forall (_ : Type_) , TypeScheme := fun (ename : Name) => fun (typ : Type_) => let allOrdVars := (encoderCollectOrdVars) (typ) in let typeVars := (encoderCollectForallVariables) (typ) in let ordVars := ((lists.filter) (fun (v : Name) => ((lists.elem) (v)) (typeVars))) (allOrdVars) in let constraints := (((logic.ifElse) ((lists.null) (ordVars))) ((None) : (option) ((list) ((prod) (Name) (TypeVariableMetadata))))) ((Some) ((maps.fromList) (((lists.map) (fun (v : Name) => (pair) (v) ((Build_TypeVariableMetadata) ((sets.singleton) ("ordering"%string))))) (ordVars)))) in let encoderFunType := ((encoderTypeNamed) (ename)) (typ) in (Build_TypeScheme) (typeVars) (encoderFunType) (constraints).
Definition encodeBinding (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Binding) , (sum) (DecodingError) (Binding) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (b : Binding) => ((eithers.bind) (((hydra.decode.core.type) (graph_)) ((fun r_ => (binding_term) (r_)) (b)))) (fun (typ : Type_) => (inr) ((Build_Binding) ((encodeBindingName) ((fun r_ => (binding_name) (r_)) (b))) (((encodeTypeNamed) ((fun r_ => (binding_name) (r_)) (b))) (typ)) ((Some) (((encoderTypeSchemeNamed) ((fun r_ => (binding_name) (r_)) (b))) (typ))))).
Arguments encodeBinding {t0}.
Definition encodeNamespace : forall (_ : Namespace) , Namespace := fun (ns : Namespace) => (strings.cat) ((cons) ("hydra.encode."%string) ((cons) (((strings.intercalate) ("."%string)) ((lists.tail) (((strings.splitOn) ("."%string)) ((fun w_ => w_) (ns))))) (nil))).
Definition isEncodableBinding : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Binding) , (sum) (Error) ((option) (Binding)) := fun (cx : Context_) => fun (graph_ : hydra.graph.Graph) => fun (b : Binding) => ((eithers.bind) ((((isSerializableByName) (cx)) (graph_)) ((fun r_ => (binding_name) (r_)) (b)))) (fun (serializable : bool) => ((inr) ((((logic.ifElse) (serializable)) ((Some) (b))) ((None) : (option) (Binding)))) : (sum) (Error) ((option) (Binding))).
Definition filterTypeBindings : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (Binding)) , (sum) (Error) ((list) (Binding)) := fun (cx : Context_) => fun (graph_ : hydra.graph.Graph) => fun (bindings : (list) (Binding)) => ((eithers.map) (maybes.cat)) (((eithers.mapList) (((isEncodableBinding) (cx)) (graph_))) (((lists.filter) (isNativeType)) (bindings))).
Definition encodeModule : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Module_) , (sum) (Error) ((option) (Module_)) := fun (cx : Context_) => fun (graph_ : hydra.graph.Graph) => fun (mod_ : Module_) => ((eithers.bind) ((((filterTypeBindings) (cx)) (graph_)) ((maybes.cat) (((lists.map) (fun (d : Definition_) => (fun x_ => match x_ with
| Definition__Type v_ => (fun (td : TypeDefinition) => (Some) (((fun (name : Name) => fun (typ : Type_) => let schemaTerm := (Term_Variable) ("hydra.core.Type"%string) in let dataTerm := (normalizeTermAnnotations) ((Term_Annotated) ((Build_AnnotatedTerm) ((hydra.encode.core.type) (typ)) ((maps.fromList) ((cons) ((pair) (key_type) (schemaTerm)) (nil))))) in (Build_Binding) (name) (dataTerm) ((Some) ((Build_TypeScheme) (nil) ((Type__Variable) ("hydra.core.Type"%string)) ((None) : (option) ((list) ((prod) (Name) (TypeVariableMetadata))))))) ((fun r_ => (typeDefinition_name) (r_)) (td))) ((fun r_ => (typeScheme_type) (r_)) ((fun r_ => (typeDefinition_type) (r_)) (td))))) (v_)
| _ => (None) : (option) (Binding)
end) (d))) ((fun r_ => (module__definitions) (r_)) (mod_)))))) (fun (typeBindings : (list) (Binding)) => (((logic.ifElse) ((lists.null) (typeBindings))) (((inr) ((None) : (option) (Module_))) : (sum) (Error) ((option) (Module_)))) (((eithers.bind) (((eithers.mapList) (fun (b : Binding) => (((eithers.bimap) (fun (_e : DecodingError) => (Error_Decoding) (_e))) (fun (x : Binding) => x)) ((((encodeBinding) (cx)) (graph_)) (b)))) (typeBindings))) (fun (encodedBindings : (list) (Binding)) => ((inr) ((Some) ((Build_Module_) ((encodeNamespace) ((fun r_ => (module__namespace) (r_)) (mod_))) (((lists.map) (fun (b : Binding) => (Definition__Term) ((Build_TermDefinition) ((fun r_ => (binding_name) (r_)) (b)) ((fun r_ => (binding_term) (r_)) (b)) ((fun r_ => (binding_type) (r_)) (b))))) (encodedBindings)) ((lists.nub) (((lists.concat2) (((lists.map) (encodeNamespace)) ((fun r_ => (module__typeDependencies) (r_)) (mod_)))) (((lists.map) (encodeNamespace)) ((fun r_ => (module__termDependencies) (r_)) (mod_))))) ((cons) ((fun r_ => (module__namespace) (r_)) (mod_)) (nil)) ((Some) ((strings.cat) ((cons) ("Term encoders for "%string) ((cons) ((fun w_ => w_) ((fun r_ => (module__namespace) (r_)) (mod_))) (nil)))))))) : (sum) (Error) ((option) (Module_))))).
Definition encoderType : forall (_ : Type_) , Type_ := fun (typ : Type_) => let resultType := (encoderFullResultType) (typ) in let baseType := (Type__Function) ((Build_FunctionType) (resultType) ((Type__Variable) ("hydra.core.Term"%string))) in ((prependForallEncoders) (baseType)) (typ).
Definition encoderTypeScheme : forall (_ : Type_) , TypeScheme := fun (typ : Type_) => let allOrdVars := (encoderCollectOrdVars) (typ) in let typeVars := (encoderCollectForallVariables) (typ) in let ordVars := ((lists.filter) (fun (v : Name) => ((lists.elem) (v)) (typeVars))) (allOrdVars) in let constraints := (((logic.ifElse) ((lists.null) (ordVars))) ((None) : (option) ((list) ((prod) (Name) (TypeVariableMetadata))))) ((Some) ((maps.fromList) (((lists.map) (fun (v : Name) => (pair) (v) ((Build_TypeVariableMetadata) ((sets.singleton) ("ordering"%string))))) (ordVars)))) in let encoderFunType := (encoderType) (typ) in (Build_TypeScheme) (typeVars) (encoderFunType) (constraints).
Definition isUnitType : forall (_ : Type_) , bool := fun x_ => match x_ with
| Type__Unit _ => true
| _ => false
end.

