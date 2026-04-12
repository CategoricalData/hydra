(* Validation functions for core terms and types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.logic hydra.lib.equality hydra.lib.lists hydra.lib.maybes hydra.lib.pairs hydra.lib.sets hydra.error.core hydra.graph hydra.variables hydra.paths hydra.lib.maps hydra.rewriting.

Definition isValidName : Name -> bool :=
  fun (name : Name) => (logic.not) (((equality.equal) ((fun w_ => w_) (name))) (""%string)).
Definition firstTypeError (t0 : Type) : (list) ((option) (t0)) -> (option) (t0) :=
  fun (checks : (list) ((option) (t0))) => (((lists.foldl) (fun (acc : (option) (t0)) => fun (check : (option) (t0)) => (((maybes.cases) (acc)) (check)) (fun (_ : t0) => acc))) (None)) (checks).
Arguments firstTypeError {t0}.
Definition firstError (t0 : Type) : (list) ((option) (t0)) -> (option) (t0) :=
  fun (checks : (list) ((option) (t0))) => (((lists.foldl) (fun (acc : (option) (t0)) => fun (check : (option) (t0)) => (((maybes.cases) (acc)) (check)) (fun (_ : t0) => acc))) (None)) (checks).
Arguments firstError {t0}.
Definition findDuplicateFieldType (t0 : Type) : (list) (t0) -> (option) (t0) :=
  fun (names : (list) (t0)) => let result := (((lists.foldl) (fun (acc : (prod) ((list) (t0)) ((option) (t0))) => fun (name : t0) => let seen := (pairs.first) (acc) in let dup := (pairs.second) (acc) in (((maybes.cases) (dup)) ((((logic.ifElse) (((sets.member) (name)) (seen))) ((pair) (seen) ((Some) (name)))) ((pair) (((sets.insert) (name)) (seen)) (None)))) (fun (_ : t0) => acc))) ((pair) (sets.empty) (None))) (names) in (pairs.second) (result).
Arguments findDuplicateFieldType {t0}.
Definition findDuplicate (t0 : Type) : (list) (t0) -> (option) (t0) :=
  fun (names : (list) (t0)) => let result := (((lists.foldl) (fun (acc : (prod) ((list) (t0)) ((option) (t0))) => fun (name : t0) => let seen := (pairs.first) (acc) in let dup := (pairs.second) (acc) in (((maybes.cases) (dup)) ((((logic.ifElse) (((sets.member) (name)) (seen))) ((pair) (seen) ((Some) (name)))) ((pair) (((sets.insert) (name)) (seen)) (None)))) (fun (_ : t0) => acc))) ((pair) (sets.empty) (None))) (names) in (pairs.second) (result).
Arguments findDuplicate {t0}.
Definition checkVoid : Type_ -> (option) (InvalidTypeError) :=
  fun (typ : Type_) => (fun x_ => match x_ with
| Type__Void _ => (Some) ((InvalidTypeError_VoidInNonBottomPosition) ((Build_VoidInNonBottomPositionError) (nil)))
| _ => None
end) (typ).
Definition checkUndefinedTypeVariablesInTypeScheme (t0 : Type) (t1 : Type) : t0 -> hydra.graph.Graph -> TypeScheme -> (Name -> (option) (t1)) -> (option) (t1) :=
  fun (path : t0) => fun (cx : hydra.graph.Graph) => fun (ts : TypeScheme) => fun (mkError : Name -> (option) (t1)) => let freeVars := (freeVariablesInTypeScheme) (ts) in let undefined := ((sets.difference) (freeVars)) ((fun r_ => (graph_typeVariables) (r_)) (cx)) in (((logic.ifElse) ((sets.null) (undefined))) (None)) (let firstUndefined := (lists.head) ((sets.toList) (undefined)) in (mkError) (firstUndefined)).
Arguments checkUndefinedTypeVariablesInTypeScheme {t0} {t1}.
Definition checkUndefinedTypeVariablesInType (t0 : Type) (t1 : Type) : t0 -> hydra.graph.Graph -> Type_ -> (Name -> (option) (t1)) -> (option) (t1) :=
  fun (path : t0) => fun (cx : hydra.graph.Graph) => fun (typ : Type_) => fun (mkError : Name -> (option) (t1)) => let freeVars := (freeVariablesInType) (typ) in let undefined := ((sets.difference) (freeVars)) ((fun r_ => (graph_typeVariables) (r_)) (cx)) in (((logic.ifElse) ((sets.null) (undefined))) (None)) (let firstUndefined := (lists.head) ((sets.toList) (undefined)) in (mkError) (firstUndefined)).
Arguments checkUndefinedTypeVariablesInType {t0} {t1}.
Definition checkShadowing : SubtermPath -> hydra.graph.Graph -> (list) (Name) -> (option) (InvalidTermError) :=
  fun (path : SubtermPath) => fun (cx : hydra.graph.Graph) => fun (names : (list) (Name)) => let result := (((lists.foldl) (fun (acc : (option) (InvalidTermError)) => fun (name : Name) => (((maybes.cases) (acc)) ((((logic.ifElse) (((logic.or) ((maybes.isJust) (((maps.lookup) (name)) ((fun r_ => (graph_boundTerms) (r_)) (cx))))) (((sets.member) (name)) ((fun r_ => (graph_lambdaVariables) (r_)) (cx))))) ((Some) ((InvalidTermError_TermVariableShadowing) ((Build_TermVariableShadowingError) (path) (name))))) (None))) (fun (_ : InvalidTermError) => acc))) (None)) (names) in result.
Definition checkDuplicateFields : SubtermPath -> (list) (Name) -> (option) (InvalidTermError) :=
  fun (path : SubtermPath) => fun (names : (list) (Name)) => let dup := (findDuplicate) (names) in ((maybes.map) (fun (name : Name) => (InvalidTermError_DuplicateField) ((Build_DuplicateFieldError) (path) (name)))) (dup).
Definition checkDuplicateFieldTypes (t0 : Type) : (list) (FieldType) -> (Name -> (option) (t0)) -> (option) (t0) :=
  fun (fields : (list) (FieldType)) => fun (mkError : Name -> (option) (t0)) => let names := ((lists.map) (fun r_ => (fieldType_name) (r_))) (fields) in let dup := (findDuplicateFieldType) (names) in (((maybes.cases) (dup)) (None)) (fun (name : Name) => (mkError) (name)).
Arguments checkDuplicateFieldTypes {t0}.
Definition validateTypeNode : (list) (Name) -> Type_ -> (option) (InvalidTypeError) :=
  fun (boundVars : (list) (Name)) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (ann : AnnotatedType) => let body := (fun r_ => (annotatedType_body) (r_)) (ann) in let annMap := (fun r_ => (annotatedType_annotation) (r_)) (ann) in (firstTypeError) ((cons) ((((logic.ifElse) ((maps.null) (annMap))) ((Some) ((InvalidTypeError_EmptyTypeAnnotation) ((Build_EmptyTypeAnnotationError) (nil))))) (None)) ((cons) ((fun x_ => match x_ with
| Type__Annotated v_ => (fun (_ : AnnotatedType) => (Some) ((InvalidTypeError_NestedTypeAnnotation) ((Build_NestedTypeAnnotationError) (nil)))) (v_)
| _ => None
end) (body)) (nil)))) (v_)
| Type__Either v_ => (fun (et : EitherType) => (firstTypeError) ((cons) ((checkVoid) ((fun r_ => (eitherType_left) (r_)) (et))) ((cons) ((checkVoid) ((fun r_ => (eitherType_right) (r_)) (et))) (nil)))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => let paramName := (fun r_ => (forallType_parameter) (r_)) (ft) in (firstTypeError) ((cons) ((((logic.ifElse) (((sets.member) (paramName)) (boundVars))) ((Some) ((InvalidTypeError_TypeVariableShadowingInForall) ((Build_TypeVariableShadowingInForallError) (nil) (paramName))))) (None)) ((cons) ((((logic.ifElse) ((isValidName) (paramName))) (None)) ((Some) ((InvalidTypeError_InvalidForallParameterName) ((Build_InvalidForallParameterNameError) (nil) (paramName))))) (nil)))) (v_)
| Type__Function v_ => (fun (ft : FunctionType) => (checkVoid) ((fun r_ => (functionType_codomain) (r_)) (ft))) (v_)
| Type__List v_ => (fun (lt : Type_) => (checkVoid) (lt)) (v_)
| Type__Map v_ => (fun (mt : MapType) => let keyType := (fun r_ => (mapType_keys) (r_)) (mt) in (firstTypeError) ((cons) ((fun x_ => match x_ with
| Type__Function v_ => (fun (_ : FunctionType) => (Some) ((InvalidTypeError_NonComparableMapKeyType) ((Build_NonComparableMapKeyTypeError) (nil) (keyType)))) (v_)
| _ => None
end) (keyType)) ((cons) ((checkVoid) (keyType)) ((cons) ((checkVoid) ((fun r_ => (mapType_values) (r_)) (mt))) (nil))))) (v_)
| Type__Pair v_ => (fun (pt : PairType) => (firstTypeError) ((cons) ((checkVoid) ((fun r_ => (pairType_first) (r_)) (pt))) ((cons) ((checkVoid) ((fun r_ => (pairType_second) (r_)) (pt))) (nil)))) (v_)
| Type__Record v_ => (fun (fields : (list) (FieldType)) => (firstTypeError) ((cons) ((((logic.ifElse) ((lists.null) (fields))) ((Some) ((InvalidTypeError_EmptyRecordType) ((Build_EmptyRecordTypeError) (nil))))) (None)) ((cons) (((checkDuplicateFieldTypes) (fields)) (fun (dupName : Name) => (Some) ((InvalidTypeError_DuplicateRecordTypeFieldNames) ((Build_DuplicateRecordTypeFieldNamesError) (nil) (dupName))))) ((cons) ((firstTypeError) (((lists.map) (fun (f : FieldType) => (checkVoid) ((fun r_ => (fieldType_type) (r_)) (f)))) (fields))) (nil))))) (v_)
| Type__Set v_ => (fun (elemType : Type_) => (firstTypeError) ((cons) ((fun x_ => match x_ with
| Type__Function v_ => (fun (_ : FunctionType) => (Some) ((InvalidTypeError_NonComparableSetElementType) ((Build_NonComparableSetElementTypeError) (nil) (elemType)))) (v_)
| _ => None
end) (elemType)) ((cons) ((checkVoid) (elemType)) (nil)))) (v_)
| Type__Union v_ => (fun (fields : (list) (FieldType)) => (firstTypeError) ((cons) ((((logic.ifElse) ((lists.null) (fields))) ((Some) ((InvalidTypeError_EmptyUnionType) ((Build_EmptyUnionTypeError) (nil))))) (None)) ((cons) ((((logic.ifElse) (((equality.equal) ((lists.length) (fields))) ((1)%Z))) (let singleField := (lists.head) (fields) in (Some) ((InvalidTypeError_SingleVariantUnion) ((Build_SingleVariantUnionError) (nil) ((fun r_ => (fieldType_name) (r_)) (singleField)))))) (None)) ((cons) (((checkDuplicateFieldTypes) (fields)) (fun (dupName : Name) => (Some) ((InvalidTypeError_DuplicateUnionTypeFieldNames) ((Build_DuplicateUnionTypeFieldNamesError) (nil) (dupName))))) ((cons) ((firstTypeError) (((lists.map) (fun (f : FieldType) => (checkVoid) ((fun r_ => (fieldType_type) (r_)) (f)))) (fields))) (nil)))))) (v_)
| Type__Variable v_ => (fun (varName : Name) => (((logic.ifElse) (((sets.member) (varName)) (boundVars))) (None)) ((Some) ((InvalidTypeError_UndefinedTypeVariable) ((Build_UndefinedTypeVariableError) (nil) (varName))))) (v_)
| _ => None
end) (typ).
Definition type_bundle :=
  hydra_fix (fun (bundle_ : (list) (Name) -> Type_ -> (option) (InvalidTypeError)) =>
    let type := bundle_ in
    fun (boundVars : (list) (Name)) => fun (typ : Type_) => let checkResult := ((validateTypeNode) (boundVars)) (typ) in (((maybes.cases) (checkResult)) ((fun x_ => match x_ with
| Type__Forall v_ => (fun (ft : ForallType) => let newBound := ((sets.insert) ((fun r_ => (forallType_parameter) (r_)) (ft))) (boundVars) in ((type) (newBound)) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| Type__Annotated v_ => (fun (ann : AnnotatedType) => ((type) (boundVars)) ((fun r_ => (annotatedType_body) (r_)) (ann))) (v_)
| Type__Application v_ => (fun (at_ : ApplicationType) => (firstTypeError) ((cons) (((type) (boundVars)) ((fun r_ => (applicationType_function) (r_)) (at_))) ((cons) (((type) (boundVars)) ((fun r_ => (applicationType_argument) (r_)) (at_))) (nil)))) (v_)
| Type__Either v_ => (fun (et : EitherType) => (firstTypeError) ((cons) (((type) (boundVars)) ((fun r_ => (eitherType_left) (r_)) (et))) ((cons) (((type) (boundVars)) ((fun r_ => (eitherType_right) (r_)) (et))) (nil)))) (v_)
| Type__Function v_ => (fun (ft : FunctionType) => (firstTypeError) ((cons) (((type) (boundVars)) ((fun r_ => (functionType_domain) (r_)) (ft))) ((cons) (((type) (boundVars)) ((fun r_ => (functionType_codomain) (r_)) (ft))) (nil)))) (v_)
| Type__List v_ => (fun (lt : Type_) => ((type) (boundVars)) (lt)) (v_)
| Type__Map v_ => (fun (mt : MapType) => (firstTypeError) ((cons) (((type) (boundVars)) ((fun r_ => (mapType_keys) (r_)) (mt))) ((cons) (((type) (boundVars)) ((fun r_ => (mapType_values) (r_)) (mt))) (nil)))) (v_)
| Type__Maybe v_ => (fun (mt : Type_) => ((type) (boundVars)) (mt)) (v_)
| Type__Pair v_ => (fun (pt : PairType) => (firstTypeError) ((cons) (((type) (boundVars)) ((fun r_ => (pairType_first) (r_)) (pt))) ((cons) (((type) (boundVars)) ((fun r_ => (pairType_second) (r_)) (pt))) (nil)))) (v_)
| Type__Record v_ => (fun (fields : (list) (FieldType)) => (firstTypeError) (((lists.map) (fun (f : FieldType) => ((type) (boundVars)) ((fun r_ => (fieldType_type) (r_)) (f)))) (fields))) (v_)
| Type__Set v_ => (fun (st : Type_) => ((type) (boundVars)) (st)) (v_)
| Type__Union v_ => (fun (fields : (list) (FieldType)) => (firstTypeError) (((lists.map) (fun (f : FieldType) => ((type) (boundVars)) ((fun r_ => (fieldType_type) (r_)) (f)))) (fields))) (v_)
| Type__Wrap v_ => (fun (wt : Type_) => ((type) (boundVars)) (wt)) (v_)
| _ => None
end) (typ))) (fun (err : InvalidTypeError) => (Some) (err))).

Definition type : (list) (Name) -> Type_ -> (option) (InvalidTypeError) :=
  type_bundle.
Definition checkDuplicateBindings : SubtermPath -> (list) (Binding) -> (option) (InvalidTermError) :=
  fun (path : SubtermPath) => fun (bindings : (list) (Binding)) => let names := ((lists.map) (fun r_ => (binding_name) (r_))) (bindings) in let dup := (findDuplicate) (names) in ((maybes.map) (fun (name : Name) => (InvalidTermError_DuplicateBinding) ((Build_DuplicateBindingError) (path) (name)))) (dup).
Definition checkTerm_term_bundle :=
  hydra_fix (fun (bundle_ : prod (bool -> SubtermPath -> hydra.graph.Graph -> Term -> (option) (InvalidTermError)) (bool -> hydra.graph.Graph -> Term -> (option) (InvalidTermError))) =>
    let checkTerm := (fst bundle_) in
    let term := (snd bundle_) in
    (pair (fun (typed : bool) => fun (path : SubtermPath) => fun (cx : hydra.graph.Graph) => fun (term_ : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (ann : AnnotatedTerm) => let body := (fun r_ => (annotatedTerm_body) (r_)) (ann) in let annMap := (fun r_ => (annotatedTerm_annotation) (r_)) (ann) in (firstError) ((cons) ((((logic.ifElse) ((maps.null) (annMap))) ((Some) ((InvalidTermError_EmptyTermAnnotation) ((Build_EmptyTermAnnotationError) (path))))) (None)) ((cons) ((fun x_ => match x_ with
| Term_Annotated v_ => (fun (_ : AnnotatedTerm) => (Some) ((InvalidTermError_NestedTermAnnotation) ((Build_NestedTermAnnotationError) (path)))) (v_)
| _ => None
end) (body)) (nil)))) (v_)
| Term_Application v_ => (fun (app : Application) => let fun_ := (fun r_ => (application_function) (r_)) (app) in let arg := (fun r_ => (application_argument) (r_)) (app) in (firstError) ((cons) ((fun x_ => match x_ with
| Term_Variable v_ => (fun (primName : Name) => (((logic.ifElse) (((equality.equal) ((fun w_ => w_) (primName))) ("logic.ifElse"%string))) ((fun x_ => match x_ with
| Term_Literal v_ => (fun (lit : Literal) => (fun x_ => match x_ with
| Literal_Boolean v_ => (fun (boolVal : bool) => (Some) ((InvalidTermError_ConstantCondition) ((Build_ConstantConditionError) (path) (boolVal)))) (v_)
| _ => None
end) (lit)) (v_)
| _ => None
end) (arg))) (None)) (v_)
| _ => None
end) (fun_)) ((cons) ((fun x_ => match x_ with
| Term_Variable v_ => (fun (funName : Name) => (fun x_ => match x_ with
| Term_Variable v_ => (fun (argName : Name) => (((logic.ifElse) (((equality.equal) (funName)) (argName))) ((Some) ((InvalidTermError_SelfApplication) ((Build_SelfApplicationError) (path) (funName))))) (None)) (v_)
| _ => None
end) (arg)) (v_)
| _ => None
end) (fun_)) ((cons) ((fun x_ => match x_ with
| Term_Function v_ => (fun (f : Function) => (fun x_ => match x_ with
| Function_Lambda v_ => (fun (lam : Lambda) => let param := (fun r_ => (lambda_parameter) (r_)) (lam) in let body := (fun r_ => (lambda_body) (r_)) (lam) in (fun x_ => match x_ with
| Term_Variable v_ => (fun (bodyVar : Name) => (((logic.ifElse) (((equality.equal) (param)) (bodyVar))) ((Some) ((InvalidTermError_UnnecessaryIdentityApplication) ((Build_UnnecessaryIdentityApplicationError) (path))))) (None)) (v_)
| _ => None
end) (body)) (v_)
| _ => None
end) (f)) (v_)
| _ => None
end) (fun_)) ((cons) ((fun x_ => match x_ with
| Term_Function v_ => (fun (f : Function) => (fun x_ => match x_ with
| Function_Elimination v_ => (fun (elim : Elimination) => (fun x_ => match x_ with
| Elimination_Wrap v_ => (fun (unwrapName : Name) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wt : WrappedTerm) => let wrapName := (fun r_ => (wrappedTerm_typeName) (r_)) (wt) in (((logic.ifElse) (((equality.equal) (unwrapName)) (wrapName))) ((Some) ((InvalidTermError_RedundantWrapUnwrap) ((Build_RedundantWrapUnwrapError) (path) (unwrapName))))) (None)) (v_)
| _ => None
end) (arg)) (v_)
| _ => None
end) (elim)) (v_)
| _ => None
end) (f)) (v_)
| _ => None
end) (fun_)) (nil)))))) (v_)
| Term_Record v_ => (fun (rec : Record_) => let tname := (fun r_ => (record__typeName) (r_)) (rec) in let flds := (fun r_ => (record__fields) (r_)) (rec) in (firstError) ((cons) ((((logic.ifElse) (((equality.equal) ((fun w_ => w_) (tname))) (""%string))) ((Some) ((InvalidTermError_EmptyTypeNameInTerm) ((Build_EmptyTypeNameInTermError) (path))))) (None)) ((cons) (((checkDuplicateFields) (path)) (((lists.map) (fun r_ => (field_name) (r_))) (flds))) (nil)))) (v_)
| Term_Let v_ => (fun (lt : Let) => let bindings := (fun r_ => (let_bindings) (r_)) (lt) in let names := ((lists.map) (fun r_ => (binding_name) (r_))) (bindings) in (firstError) ((cons) ((((logic.ifElse) ((lists.null) (bindings))) ((Some) ((InvalidTermError_EmptyLetBindings) ((Build_EmptyLetBindingsError) (path))))) (None)) ((cons) (((checkDuplicateBindings) (path)) (bindings)) ((cons) (None) ((cons) ((firstError) (((lists.map) (fun (bname : Name) => (((logic.ifElse) ((isValidName) (bname))) (None)) ((Some) ((InvalidTermError_InvalidLetBindingName) ((Build_InvalidLetBindingNameError) (path) (bname)))))) (names))) ((cons) ((((logic.ifElse) (typed)) ((firstError) (((lists.map) (fun (b : Binding) => (((maybes.cases) ((fun r_ => (binding_type) (r_)) (b))) (None)) (fun (ts : TypeScheme) => ((((checkUndefinedTypeVariablesInTypeScheme) (path)) (cx)) (ts)) (fun (uvName : Name) => (Some) ((InvalidTermError_UndefinedTypeVariableInBindingType) ((Build_UndefinedTypeVariableInBindingTypeError) (path) (uvName))))))) (bindings)))) (None)) (nil))))))) (v_)
| Term_Union v_ => (fun (inj : Injection) => let tname := (fun r_ => (injection_typeName) (r_)) (inj) in (((logic.ifElse) (((equality.equal) ((fun w_ => w_) (tname))) (""%string))) ((Some) ((InvalidTermError_EmptyTypeNameInTerm) ((Build_EmptyTypeNameInTermError) (path))))) (None)) (v_)
| Term_Function v_ => (fun (fun_ : Function) => (fun x_ => match x_ with
| Function_Lambda v_ => (fun (lam : Lambda) => let paramName := (fun r_ => (lambda_parameter) (r_)) (lam) in (firstError) ((cons) ((((logic.ifElse) ((maybes.isJust) (((maps.lookup) (paramName)) ((fun r_ => (graph_boundTerms) (r_)) (cx))))) ((Some) ((InvalidTermError_TermVariableShadowing) ((Build_TermVariableShadowingError) (path) (paramName))))) (None)) ((cons) ((((logic.ifElse) ((isValidName) (paramName))) (None)) ((Some) ((InvalidTermError_InvalidLambdaParameterName) ((Build_InvalidLambdaParameterNameError) (path) (paramName))))) ((cons) ((((logic.ifElse) (typed)) ((((maybes.cases) ((fun r_ => (lambda_domain) (r_)) (lam))) (None)) (fun (dom : Type_) => ((((checkUndefinedTypeVariablesInType) (path)) (cx)) (dom)) (fun (uvName : Name) => (Some) ((InvalidTermError_UndefinedTypeVariableInLambdaDomain) ((Build_UndefinedTypeVariableInLambdaDomainError) (path) (uvName))))))) (None)) (nil))))) (v_)
| Function_Elimination v_ => (fun (elim : Elimination) => (fun x_ => match x_ with
| Elimination_Record v_ => (fun (proj : Projection) => let tname := (fun r_ => (projection_typeName) (r_)) (proj) in (((logic.ifElse) (((equality.equal) ((fun w_ => w_) (tname))) (""%string))) ((Some) ((InvalidTermError_EmptyTypeNameInTerm) ((Build_EmptyTypeNameInTermError) (path))))) (None)) (v_)
| Elimination_Union v_ => (fun (cs : CaseStatement) => let tname := (fun r_ => (caseStatement_typeName) (r_)) (cs) in let csDefault := (fun r_ => (caseStatement_default) (r_)) (cs) in let csCases := (fun r_ => (caseStatement_cases) (r_)) (cs) in (firstError) ((cons) ((((logic.ifElse) (((equality.equal) ((fun w_ => w_) (tname))) (""%string))) ((Some) ((InvalidTermError_EmptyTypeNameInTerm) ((Build_EmptyTypeNameInTermError) (path))))) (None)) ((cons) ((((logic.ifElse) (((logic.and) ((lists.null) (csCases))) ((maybes.isNothing) (csDefault)))) ((Some) ((InvalidTermError_EmptyCaseStatement) ((Build_EmptyCaseStatementError) (path) (tname))))) (None)) ((cons) (((checkDuplicateFields) (path)) (((lists.map) (fun r_ => (field_name) (r_))) (csCases))) (nil))))) (v_)
| _ => None
end) (elim)) (v_)
end) (fun_)) (v_)
| Term_TypeApplication v_ => (fun (ta : TypeApplicationTerm) => (((logic.ifElse) (typed)) (((((checkUndefinedTypeVariablesInType) (path)) (cx)) ((fun r_ => (typeApplicationTerm_type) (r_)) (ta))) (fun (uvName : Name) => (Some) ((InvalidTermError_UndefinedTypeVariableInTypeApplication) ((Build_UndefinedTypeVariableInTypeApplicationError) (path) (uvName)))))) (None)) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => let tvName := (fun r_ => (typeLambda_parameter) (r_)) (tl) in (firstError) ((cons) ((((logic.ifElse) (((sets.member) (tvName)) (((sets.delete) (tvName)) ((fun r_ => (graph_typeVariables) (r_)) (cx))))) ((Some) ((InvalidTermError_TypeVariableShadowingInTypeLambda) ((Build_TypeVariableShadowingInTypeLambdaError) (path) (tvName))))) (None)) ((cons) ((((logic.ifElse) ((isValidName) (tvName))) (None)) ((Some) ((InvalidTermError_InvalidTypeLambdaParameterName) ((Build_InvalidTypeLambdaParameterNameError) (path) (tvName))))) (nil)))) (v_)
| Term_Variable v_ => (fun (varName : Name) => (((logic.ifElse) (((logic.or) ((maybes.isJust) (((maps.lookup) (varName)) ((fun r_ => (graph_boundTerms) (r_)) (cx))))) (((logic.or) (((sets.member) (varName)) ((fun r_ => (graph_lambdaVariables) (r_)) (cx)))) ((maybes.isJust) (((maps.lookup) (varName)) ((fun r_ => (graph_primitives) (r_)) (cx))))))) (None)) ((Some) ((InvalidTermError_UndefinedTermVariable) ((Build_UndefinedTermVariableError) (path) (varName))))) (v_)
| Term_Wrap v_ => (fun (wt : WrappedTerm) => let tname := (fun r_ => (wrappedTerm_typeName) (r_)) (wt) in (((logic.ifElse) (((equality.equal) ((fun w_ => w_) (tname))) (""%string))) ((Some) ((InvalidTermError_EmptyTypeNameInTerm) ((Build_EmptyTypeNameInTermError) (path))))) (None)) (v_)
| _ => None
end) (term_)) (fun (typed : bool) => fun (g : hydra.graph.Graph) => fun (t : Term) => ((((foldTermWithGraphAndPath) (fun (recurse : (option) (InvalidTermError) -> Term -> (option) (InvalidTermError)) => fun (path : (list) (SubtermStep)) => fun (cx : hydra.graph.Graph) => fun (acc : (option) (InvalidTermError)) => fun (trm : Term) => (((maybes.cases) (acc)) (let checkResult := ((((checkTerm) (typed)) (path)) (cx)) (trm) in (((maybes.cases) (checkResult)) (((recurse) (None)) (trm))) (fun (err : InvalidTermError) => (Some) (err)))) (fun (_ : InvalidTermError) => acc))) (g)) (None)) (t)))).

Definition checkTerm : bool -> SubtermPath -> hydra.graph.Graph -> Term -> (option) (InvalidTermError) :=
  (fst checkTerm_term_bundle).
Definition term : bool -> hydra.graph.Graph -> Term -> (option) (InvalidTermError) :=
  (snd checkTerm_term_bundle).

