(* Term encoders for hydra.core *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.eithers hydra.lib.lists hydra.lib.maps hydra.lib.maybes hydra.lib.pairs hydra.lib.sets.

Definition name : forall (_ : Name) , Term := fun (x : Name) => (Term_Wrap) ((Build_WrappedTerm) ("hydra.core.Name"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition floatType : forall (_ : FloatType) , Term := fun x_ => match x_ with
| FloatType_Bigfloat v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.FloatType"%string) ((Build_Field) ("bigfloat"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| FloatType_Float32 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.FloatType"%string) ((Build_Field) ("float32"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| FloatType_Float64 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.FloatType"%string) ((Build_Field) ("float64"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition integerType : forall (_ : IntegerType) , Term := fun x_ => match x_ with
| IntegerType_Bigint v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("bigint"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| IntegerType_Int8 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("int8"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| IntegerType_Int16 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("int16"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| IntegerType_Int32 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("int32"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| IntegerType_Int64 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("int64"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| IntegerType_Uint8 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("uint8"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| IntegerType_Uint16 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("uint16"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| IntegerType_Uint32 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("uint32"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| IntegerType_Uint64 v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerType"%string) ((Build_Field) ("uint64"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition literalType : forall (_ : LiteralType) , Term := fun x_ => match x_ with
| LiteralType_Binary v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.LiteralType"%string) ((Build_Field) ("binary"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| LiteralType_Boolean v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.LiteralType"%string) ((Build_Field) ("boolean"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| LiteralType_Decimal v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.LiteralType"%string) ((Build_Field) ("decimal"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| LiteralType_Float v_ => (fun (y : FloatType) => (Term_Inject) ((Build_Injection) ("hydra.core.LiteralType"%string) ((Build_Field) ("float"%string) ((floatType) (y))))) (v_)
| LiteralType_Integer v_ => (fun (y : IntegerType) => (Term_Inject) ((Build_Injection) ("hydra.core.LiteralType"%string) ((Build_Field) ("integer"%string) ((integerType) (y))))) (v_)
| LiteralType_String v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.LiteralType"%string) ((Build_Field) ("string"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition typeVariableMetadata : forall (_ : TypeVariableMetadata) , Term := fun (x : TypeVariableMetadata) => (Term_Record) ((Build_Record_) ("hydra.core.TypeVariableMetadata"%string) ((cons) ((Build_Field) ("classes"%string) ((fun (s : (list) (Name)) => (Term_Set) (((sets.map) (name)) (s))) ((fun r_ => (typeVariableMetadata_classes) (r_)) (x)))) (nil))).
Definition floatValue : forall (_ : FloatValue) , Term := fun x_ => match x_ with
| FloatValue_Bigfloat v_ => (fun (y : Q) => (Term_Inject) ((Build_Injection) ("hydra.core.FloatValue"%string) ((Build_Field) ("bigfloat"%string) ((fun (x : Q) => (Term_Literal) ((Literal_Float) ((FloatValue_Bigfloat) (x)))) (y))))) (v_)
| FloatValue_Float32 v_ => (fun (y : Q) => (Term_Inject) ((Build_Injection) ("hydra.core.FloatValue"%string) ((Build_Field) ("float32"%string) ((fun (x : Q) => (Term_Literal) ((Literal_Float) ((FloatValue_Float32) (x)))) (y))))) (v_)
| FloatValue_Float64 v_ => (fun (y : Q) => (Term_Inject) ((Build_Injection) ("hydra.core.FloatValue"%string) ((Build_Field) ("float64"%string) ((fun (x : Q) => (Term_Literal) ((Literal_Float) ((FloatValue_Float64) (x)))) (y))))) (v_)
end.
Definition integerValue : forall (_ : IntegerValue) , Term := fun x_ => match x_ with
| IntegerValue_Bigint v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("bigint"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Bigint) (x)))) (y))))) (v_)
| IntegerValue_Int8 v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("int8"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int8) (x)))) (y))))) (v_)
| IntegerValue_Int16 v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("int16"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int16) (x)))) (y))))) (v_)
| IntegerValue_Int32 v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("int32"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
| IntegerValue_Int64 v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("int64"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int64) (x)))) (y))))) (v_)
| IntegerValue_Uint8 v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("uint8"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Uint8) (x)))) (y))))) (v_)
| IntegerValue_Uint16 v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("uint16"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Uint16) (x)))) (y))))) (v_)
| IntegerValue_Uint32 v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("uint32"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Uint32) (x)))) (y))))) (v_)
| IntegerValue_Uint64 v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("hydra.core.IntegerValue"%string) ((Build_Field) ("uint64"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Uint64) (x)))) (y))))) (v_)
end.
Definition literal : forall (_ : Literal) , Term := fun x_ => match x_ with
| Literal_Binary v_ => (fun (y : string) => (Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("binary"%string) ((fun (x : string) => (Term_Literal) ((Literal_Binary) (x))) (y))))) (v_)
| Literal_Boolean v_ => (fun (y : bool) => (Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("boolean"%string) ((fun (x : bool) => (Term_Literal) ((Literal_Boolean) (x))) (y))))) (v_)
| Literal_Decimal v_ => (fun (y : Q) => (Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("decimal"%string) ((fun (x : Q) => (Term_Literal) ((Literal_Decimal) (x))) (y))))) (v_)
| Literal_Float v_ => (fun (y : FloatValue) => (Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("float"%string) ((floatValue) (y))))) (v_)
| Literal_Integer v_ => (fun (y : IntegerValue) => (Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("integer"%string) ((integerValue) (y))))) (v_)
| Literal_String v_ => (fun (y : string) => (Term_Inject) ((Build_Injection) ("hydra.core.Literal"%string) ((Build_Field) ("string"%string) ((fun (x : string) => (Term_Literal) ((Literal_String) (x))) (y))))) (v_)
end.
Definition projection : forall (_ : Projection) , Term := fun (x : Projection) => (Term_Record) ((Build_Record_) ("hydra.core.Projection"%string) ((cons) ((Build_Field) ("typeName"%string) ((name) ((fun r_ => (projection_typeName) (r_)) (x)))) ((cons) ((Build_Field) ("field"%string) ((name) ((fun r_ => (projection_field) (r_)) (x)))) (nil)))).
Definition annotatedTerm_annotatedType_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : AnnotatedTerm) , Term) (prod (forall (_ : AnnotatedType) , Term) (prod (forall (_ : Application) , Term) (prod (forall (_ : ApplicationType) , Term) (prod (forall (_ : Binding) , Term) (prod (forall (_ : CaseStatement) , Term) (prod (forall (_ : EitherType) , Term) (prod (forall (_ : Field) , Term) (prod (forall (_ : FieldType) , Term) (prod (forall (_ : ForallType) , Term) (prod (forall (_ : FunctionType) , Term) (prod (forall (_ : Injection) , Term) (prod (forall (_ : Lambda) , Term) (prod (forall (_ : Let) , Term) (prod (forall (_ : MapType) , Term) (prod (forall (_ : PairType) , Term) (prod (forall (_ : Record_) , Term) (prod (forall (_ : Term) , Term) (prod (forall (_ : Type_) , Term) (prod (forall (_ : TypeApplicationTerm) , Term) (prod (forall (_ : TypeLambda) , Term) (prod (forall (_ : TypeScheme) , Term) (forall (_ : WrappedTerm) , Term))))))))))))))))))))))) =>
    let annotatedTerm := (fst bundle_) in
    let annotatedType := (fst (snd bundle_)) in
    let application := (fst (snd (snd bundle_))) in
    let applicationType := (fst (snd (snd (snd bundle_)))) in
    let binding := (fst (snd (snd (snd (snd bundle_))))) in
    let caseStatement := (fst (snd (snd (snd (snd (snd bundle_)))))) in
    let eitherType := (fst (snd (snd (snd (snd (snd (snd bundle_))))))) in
    let field := (fst (snd (snd (snd (snd (snd (snd (snd bundle_)))))))) in
    let fieldType := (fst (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))) in
    let forallType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))) in
    let functionType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))) in
    let injection := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))) in
    let lambda := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))) in
    let let_ := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))) in
    let mapType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))) in
    let pairType := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))) in
    let record := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))) in
    let term := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))) in
    let type := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))) in
    let typeApplicationTerm := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))) in
    let typeLambda := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_))))))))))))))))))))) in
    let typeScheme := (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))))) in
    let wrappedTerm := (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd bundle_)))))))))))))))))))))) in
    (pair (fun (x : AnnotatedTerm) => (Term_Record) ((Build_Record_) ("hydra.core.AnnotatedTerm"%string) ((cons) ((Build_Field) ("body"%string) ((term) ((fun r_ => (annotatedTerm_body) (r_)) (x)))) ((cons) ((Build_Field) ("annotation"%string) ((fun (m : (list) ((prod) (Name) (Term))) => (Term_Map) ((((maps.bimap) (name)) (term)) (m))) ((fun r_ => (annotatedTerm_annotation) (r_)) (x)))) (nil))))) ((pair (fun (x : AnnotatedType) => (Term_Record) ((Build_Record_) ("hydra.core.AnnotatedType"%string) ((cons) ((Build_Field) ("body"%string) ((type) ((fun r_ => (annotatedType_body) (r_)) (x)))) ((cons) ((Build_Field) ("annotation"%string) ((fun (m : (list) ((prod) (Name) (Term))) => (Term_Map) ((((maps.bimap) (name)) (term)) (m))) ((fun r_ => (annotatedType_annotation) (r_)) (x)))) (nil))))) ((pair (fun (x : Application) => (Term_Record) ((Build_Record_) ("hydra.core.Application"%string) ((cons) ((Build_Field) ("function"%string) ((term) ((fun r_ => (application_function) (r_)) (x)))) ((cons) ((Build_Field) ("argument"%string) ((term) ((fun r_ => (application_argument) (r_)) (x)))) (nil))))) ((pair (fun (x : ApplicationType) => (Term_Record) ((Build_Record_) ("hydra.core.ApplicationType"%string) ((cons) ((Build_Field) ("function"%string) ((type) ((fun r_ => (applicationType_function) (r_)) (x)))) ((cons) ((Build_Field) ("argument"%string) ((type) ((fun r_ => (applicationType_argument) (r_)) (x)))) (nil))))) ((pair (fun (x : Binding) => (Term_Record) ((Build_Record_) ("hydra.core.Binding"%string) ((cons) ((Build_Field) ("name"%string) ((name) ((fun r_ => (binding_name) (r_)) (x)))) ((cons) ((Build_Field) ("term"%string) ((term) ((fun r_ => (binding_term) (r_)) (x)))) ((cons) ((Build_Field) ("type"%string) ((fun (opt : (option) (TypeScheme)) => (Term_Maybe) (((maybes.map) (typeScheme)) (opt))) ((fun r_ => (binding_type) (r_)) (x)))) (nil)))))) ((pair (fun (x : CaseStatement) => (Term_Record) ((Build_Record_) ("hydra.core.CaseStatement"%string) ((cons) ((Build_Field) ("typeName"%string) ((name) ((fun r_ => (caseStatement_typeName) (r_)) (x)))) ((cons) ((Build_Field) ("default"%string) ((fun (opt : (option) (Term)) => (Term_Maybe) (((maybes.map) (term)) (opt))) ((fun r_ => (caseStatement_default) (r_)) (x)))) ((cons) ((Build_Field) ("cases"%string) ((fun (xs : (list) (Field)) => (Term_List) (((lists.map) (field)) (xs))) ((fun r_ => (caseStatement_cases) (r_)) (x)))) (nil)))))) ((pair (fun (x : EitherType) => (Term_Record) ((Build_Record_) ("hydra.core.EitherType"%string) ((cons) ((Build_Field) ("left"%string) ((type) ((fun r_ => (eitherType_left) (r_)) (x)))) ((cons) ((Build_Field) ("right"%string) ((type) ((fun r_ => (eitherType_right) (r_)) (x)))) (nil))))) ((pair (fun (x : Field) => (Term_Record) ((Build_Record_) ("hydra.core.Field"%string) ((cons) ((Build_Field) ("name"%string) ((name) ((fun r_ => (field_name) (r_)) (x)))) ((cons) ((Build_Field) ("term"%string) ((term) ((fun r_ => (field_term) (r_)) (x)))) (nil))))) ((pair (fun (x : FieldType) => (Term_Record) ((Build_Record_) ("hydra.core.FieldType"%string) ((cons) ((Build_Field) ("name"%string) ((name) ((fun r_ => (fieldType_name) (r_)) (x)))) ((cons) ((Build_Field) ("type"%string) ((type) ((fun r_ => (fieldType_type) (r_)) (x)))) (nil))))) ((pair (fun (x : ForallType) => (Term_Record) ((Build_Record_) ("hydra.core.ForallType"%string) ((cons) ((Build_Field) ("parameter"%string) ((name) ((fun r_ => (forallType_parameter) (r_)) (x)))) ((cons) ((Build_Field) ("body"%string) ((type) ((fun r_ => (forallType_body) (r_)) (x)))) (nil))))) ((pair (fun (x : FunctionType) => (Term_Record) ((Build_Record_) ("hydra.core.FunctionType"%string) ((cons) ((Build_Field) ("domain"%string) ((type) ((fun r_ => (functionType_domain) (r_)) (x)))) ((cons) ((Build_Field) ("codomain"%string) ((type) ((fun r_ => (functionType_codomain) (r_)) (x)))) (nil))))) ((pair (fun (x : Injection) => (Term_Record) ((Build_Record_) ("hydra.core.Injection"%string) ((cons) ((Build_Field) ("typeName"%string) ((name) ((fun r_ => (injection_typeName) (r_)) (x)))) ((cons) ((Build_Field) ("field"%string) ((field) ((fun r_ => (injection_field) (r_)) (x)))) (nil))))) ((pair (fun (x : Lambda) => (Term_Record) ((Build_Record_) ("hydra.core.Lambda"%string) ((cons) ((Build_Field) ("parameter"%string) ((name) ((fun r_ => (lambda_parameter) (r_)) (x)))) ((cons) ((Build_Field) ("domain"%string) ((fun (opt : (option) (Type_)) => (Term_Maybe) (((maybes.map) (type)) (opt))) ((fun r_ => (lambda_domain) (r_)) (x)))) ((cons) ((Build_Field) ("body"%string) ((term) ((fun r_ => (lambda_body) (r_)) (x)))) (nil)))))) ((pair (fun (x : Let) => (Term_Record) ((Build_Record_) ("hydra.core.Let"%string) ((cons) ((Build_Field) ("bindings"%string) ((fun (xs : (list) (Binding)) => (Term_List) (((lists.map) (binding)) (xs))) ((fun r_ => (let_bindings) (r_)) (x)))) ((cons) ((Build_Field) ("body"%string) ((term) ((fun r_ => (let_body) (r_)) (x)))) (nil))))) ((pair (fun (x : MapType) => (Term_Record) ((Build_Record_) ("hydra.core.MapType"%string) ((cons) ((Build_Field) ("keys"%string) ((type) ((fun r_ => (mapType_keys) (r_)) (x)))) ((cons) ((Build_Field) ("values"%string) ((type) ((fun r_ => (mapType_values) (r_)) (x)))) (nil))))) ((pair (fun (x : PairType) => (Term_Record) ((Build_Record_) ("hydra.core.PairType"%string) ((cons) ((Build_Field) ("first"%string) ((type) ((fun r_ => (pairType_first) (r_)) (x)))) ((cons) ((Build_Field) ("second"%string) ((type) ((fun r_ => (pairType_second) (r_)) (x)))) (nil))))) ((pair (fun (x : Record_) => (Term_Record) ((Build_Record_) ("hydra.core.Record"%string) ((cons) ((Build_Field) ("typeName"%string) ((name) ((fun r_ => (record__typeName) (r_)) (x)))) ((cons) ((Build_Field) ("fields"%string) ((fun (xs : (list) (Field)) => (Term_List) (((lists.map) (field)) (xs))) ((fun r_ => (record__fields) (r_)) (x)))) (nil))))) ((pair (fun x_ => match x_ with
| Term_Annotated v_ => (fun (y : AnnotatedTerm) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("annotated"%string) ((annotatedTerm) (y))))) (v_)
| Term_Application v_ => (fun (y : Application) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("application"%string) ((application) (y))))) (v_)
| Term_Cases v_ => (fun (y : CaseStatement) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("cases"%string) ((caseStatement) (y))))) (v_)
| Term_Either v_ => (fun (y : (sum) (Term) (Term)) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("either"%string) ((fun (e : (sum) (Term) (Term)) => (Term_Either) ((((eithers.bimap) (term)) (term)) (e))) (y))))) (v_)
| Term_Inject v_ => (fun (y : Injection) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("inject"%string) ((injection) (y))))) (v_)
| Term_Lambda v_ => (fun (y : Lambda) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("lambda"%string) ((lambda) (y))))) (v_)
| Term_Let v_ => (fun (y : Let) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("let"%string) ((let_) (y))))) (v_)
| Term_List v_ => (fun (y : (list) (Term)) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("list"%string) ((fun (xs : (list) (Term)) => (Term_List) (((lists.map) (term)) (xs))) (y))))) (v_)
| Term_Literal v_ => (fun (y : Literal) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("literal"%string) ((literal) (y))))) (v_)
| Term_Map v_ => (fun (y : (list) ((prod) (Term) (Term))) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("map"%string) ((fun (m : (list) ((prod) (Term) (Term))) => (Term_Map) ((((maps.bimap) (term)) (term)) (m))) (y))))) (v_)
| Term_Maybe v_ => (fun (y : (option) (Term)) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("maybe"%string) ((fun (opt : (option) (Term)) => (Term_Maybe) (((maybes.map) (term)) (opt))) (y))))) (v_)
| Term_Pair v_ => (fun (y : (prod) (Term) (Term)) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("pair"%string) ((fun (p : (prod) (Term) (Term)) => (Term_Pair) ((((pairs.bimap) (term)) (term)) (p))) (y))))) (v_)
| Term_Project v_ => (fun (y : Projection) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("project"%string) ((projection) (y))))) (v_)
| Term_Record v_ => (fun (y : Record_) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("record"%string) ((record) (y))))) (v_)
| Term_Set v_ => (fun (y : (list) (Term)) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("set"%string) ((fun (s : (list) (Term)) => (Term_Set) (((sets.map) (term)) (s))) (y))))) (v_)
| Term_TypeApplication v_ => (fun (y : TypeApplicationTerm) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("typeApplication"%string) ((typeApplicationTerm) (y))))) (v_)
| Term_TypeLambda v_ => (fun (y : TypeLambda) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("typeLambda"%string) ((typeLambda) (y))))) (v_)
| Term_Unit v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("unit"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Term_Unwrap v_ => (fun (y : Name) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("unwrap"%string) ((name) (y))))) (v_)
| Term_Variable v_ => (fun (y : Name) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("variable"%string) ((name) (y))))) (v_)
| Term_Wrap v_ => (fun (y : WrappedTerm) => (Term_Inject) ((Build_Injection) ("hydra.core.Term"%string) ((Build_Field) ("wrap"%string) ((wrappedTerm) (y))))) (v_)
end) ((pair (fun x_ => match x_ with
| Type__Annotated v_ => (fun (y : AnnotatedType) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("annotated"%string) ((annotatedType) (y))))) (v_)
| Type__Application v_ => (fun (y : ApplicationType) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("application"%string) ((applicationType) (y))))) (v_)
| Type__Either v_ => (fun (y : EitherType) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("either"%string) ((eitherType) (y))))) (v_)
| Type__Forall v_ => (fun (y : ForallType) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("forall"%string) ((forallType) (y))))) (v_)
| Type__Function v_ => (fun (y : FunctionType) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("function"%string) ((functionType) (y))))) (v_)
| Type__List v_ => (fun (y : Type_) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("list"%string) ((type) (y))))) (v_)
| Type__Literal v_ => (fun (y : LiteralType) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("literal"%string) ((literalType) (y))))) (v_)
| Type__Map v_ => (fun (y : MapType) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("map"%string) ((mapType) (y))))) (v_)
| Type__Maybe v_ => (fun (y : Type_) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("maybe"%string) ((type) (y))))) (v_)
| Type__Pair v_ => (fun (y : PairType) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("pair"%string) ((pairType) (y))))) (v_)
| Type__Record v_ => (fun (y : (list) (FieldType)) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("record"%string) ((fun (xs : (list) (FieldType)) => (Term_List) (((lists.map) (fieldType)) (xs))) (y))))) (v_)
| Type__Set v_ => (fun (y : Type_) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("set"%string) ((type) (y))))) (v_)
| Type__Union v_ => (fun (y : (list) (FieldType)) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("union"%string) ((fun (xs : (list) (FieldType)) => (Term_List) (((lists.map) (fieldType)) (xs))) (y))))) (v_)
| Type__Unit v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("unit"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Type__Variable v_ => (fun (y : Name) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("variable"%string) ((name) (y))))) (v_)
| Type__Void v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("void"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Type__Wrap v_ => (fun (y : Type_) => (Term_Inject) ((Build_Injection) ("hydra.core.Type"%string) ((Build_Field) ("wrap"%string) ((type) (y))))) (v_)
end) ((pair (fun (x : TypeApplicationTerm) => (Term_Record) ((Build_Record_) ("hydra.core.TypeApplicationTerm"%string) ((cons) ((Build_Field) ("body"%string) ((term) ((fun r_ => (typeApplicationTerm_body) (r_)) (x)))) ((cons) ((Build_Field) ("type"%string) ((type) ((fun r_ => (typeApplicationTerm_type) (r_)) (x)))) (nil))))) ((pair (fun (x : TypeLambda) => (Term_Record) ((Build_Record_) ("hydra.core.TypeLambda"%string) ((cons) ((Build_Field) ("parameter"%string) ((name) ((fun r_ => (typeLambda_parameter) (r_)) (x)))) ((cons) ((Build_Field) ("body"%string) ((term) ((fun r_ => (typeLambda_body) (r_)) (x)))) (nil))))) ((pair (fun (x : TypeScheme) => (Term_Record) ((Build_Record_) ("hydra.core.TypeScheme"%string) ((cons) ((Build_Field) ("variables"%string) ((fun (xs : (list) (Name)) => (Term_List) (((lists.map) (name)) (xs))) ((fun r_ => (typeScheme_variables) (r_)) (x)))) ((cons) ((Build_Field) ("type"%string) ((type) ((fun r_ => (typeScheme_type) (r_)) (x)))) ((cons) ((Build_Field) ("constraints"%string) ((fun (opt : (option) ((list) ((prod) (Name) (TypeVariableMetadata)))) => (Term_Maybe) (((maybes.map) (fun (m : (list) ((prod) (Name) (TypeVariableMetadata))) => (Term_Map) ((((maps.bimap) (name)) (typeVariableMetadata)) (m)))) (opt))) ((fun r_ => (typeScheme_constraints) (r_)) (x)))) (nil)))))) (fun (x : WrappedTerm) => (Term_Record) ((Build_Record_) ("hydra.core.WrappedTerm"%string) ((cons) ((Build_Field) ("typeName"%string) ((name) ((fun r_ => (wrappedTerm_typeName) (r_)) (x)))) ((cons) ((Build_Field) ("body"%string) ((term) ((fun r_ => (wrappedTerm_body) (r_)) (x)))) (nil))))))))))))))))))))))))))))))))))))))))))))))))).

Definition annotatedTerm : forall (_ : AnnotatedTerm) , Term :=
  (fst annotatedTerm_annotatedType_bundle).
Definition annotatedType : forall (_ : AnnotatedType) , Term :=
  (fst (snd annotatedTerm_annotatedType_bundle)).
Definition application : forall (_ : Application) , Term :=
  (fst (snd (snd annotatedTerm_annotatedType_bundle))).
Definition applicationType : forall (_ : ApplicationType) , Term :=
  (fst (snd (snd (snd annotatedTerm_annotatedType_bundle)))).
Definition binding : forall (_ : Binding) , Term :=
  (fst (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))).
Definition caseStatement : forall (_ : CaseStatement) , Term :=
  (fst (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))).
Definition eitherType : forall (_ : EitherType) , Term :=
  (fst (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))).
Definition field : forall (_ : Field) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))).
Definition fieldType : forall (_ : FieldType) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))).
Definition forallType : forall (_ : ForallType) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))).
Definition functionType : forall (_ : FunctionType) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))).
Definition injection : forall (_ : Injection) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))).
Definition lambda : forall (_ : Lambda) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))).
Definition let_ : forall (_ : Let) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))).
Definition mapType : forall (_ : MapType) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))).
Definition pairType : forall (_ : PairType) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))).
Definition record : forall (_ : Record_) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))).
Definition term : forall (_ : Term) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))).
Definition type : forall (_ : Type_) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))).
Definition typeApplicationTerm : forall (_ : TypeApplicationTerm) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))).
Definition typeLambda : forall (_ : TypeLambda) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle))))))))))))))))))))).
Definition typeScheme : forall (_ : TypeScheme) , Term :=
  (fst (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))))).
Definition wrappedTerm : forall (_ : WrappedTerm) , Term :=
  (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd (snd annotatedTerm_annotatedType_bundle)))))))))))))))))))))).

