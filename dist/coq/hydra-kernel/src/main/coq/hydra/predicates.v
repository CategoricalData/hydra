(* Type and term classification predicates *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.context hydra.graph hydra.core hydra.errors hydra.lib.lists hydra.lib.strings hydra.lib.eithers hydra.lexical hydra.decode.core hydra.lib.logic hydra.lib.sets hydra.lib.maps hydra.dependencies hydra.lib.pairs hydra.lib.equality hydra.strip hydra.lib.maybes hydra.reflect hydra.rewriting hydra.coders hydra.variants hydra.arity.

Definition typeDependencies : Context_ -> hydra.graph.Graph -> bool -> (Type_ -> Type_) -> Name -> (sum) (Error) ((list) ((prod) (Name) (Type_))) :=
  fun (cx : Context_) => fun (graph_ : hydra.graph.Graph) => fun (withSchema : bool) => fun (transform : Type_ -> Type_) => fun (name : Name) => let requireType := fun (name2 : Name) => let cx1 := (Build_Context_) (((lists.cons) (((strings.cat2) ("type dependencies of "%string)) ((fun w_ => w_) (name2)))) ((fun r_ => (context__trace) (r_)) (cx))) ((fun r_ => (context__messages) (r_)) (cx)) ((fun r_ => (context__other) (r_)) (cx)) in ((eithers.bind) (((requireBinding) (graph_)) (name2))) (fun (el : Binding) => (((eithers.bimap) (fun (_e : DecodingError) => (Error_Decoding) (_e))) (fun (_a : Type_) => _a)) (((hydra.decode.core.type) (graph_)) ((fun r_ => (binding_term) (r_)) (el)))) in let toPair := fun (name2 : Name) => ((eithers.map) (fun (typ : Type_) => (pair) (name2) ((transform) (typ)))) ((requireType) (name2)) in let deps := (hydra_fix) (fun deps => fun (seeds : (list) (Name)) => fun (names : (list) ((prod) (Name) (Type_))) => (((logic.ifElse) ((sets.null) (seeds))) ((inr) (names))) (((eithers.bind) (((eithers.mapList) (toPair)) ((sets.toList) (seeds)))) (fun (pairs : (list) ((prod) (Name) (Type_))) => let visited := (sets.fromList) ((maps.keys) (names)) in let refs := (((lists.foldl) (sets.union)) (sets.empty)) (((lists.map) (fun (pair_ : (prod) (Name) (Type_)) => ((typeDependencyNames) (withSchema)) ((pairs.second) (pair_)))) (pairs)) in let newSeeds := ((sets.difference) (refs)) (visited) in let newNames := ((maps.union) (names)) ((maps.fromList) (pairs)) in ((deps) (newSeeds)) (newNames)))) in ((deps) ((sets.singleton) (name))) (maps.empty).
Definition isUnitType : Type_ -> bool :=
  fun x_ => match x_ with
| Type__Unit _ => true
| _ => false
end.
Definition isUnitTerm : Term -> bool :=
  fun x_ => match x_ with
| Term_Unit _ => true
| _ => false
end.
Definition isType_bundle :=
  hydra_fix (fun (bundle_ : Type_ -> bool) =>
    let isType := bundle_ in
    fun (t : Type_) => (fun x_ => match x_ with
| Type__Application v_ => (fun (a : ApplicationType) => (isType) ((fun r_ => (applicationType_function) (r_)) (a))) (v_)
| Type__Forall v_ => (fun (l : ForallType) => (isType) ((fun r_ => (forallType_body) (r_)) (l))) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => false) (v_)
| Type__Variable v_ => (fun (v : Name) => ((equality.equal) (v)) ("Type_"%string)) (v_)
| _ => false
end) ((deannotateType) (t))).

Definition isType : Type_ -> bool :=
  isType_bundle.
Definition isTrivialTerm_bundle :=
  hydra_fix (fun (bundle_ : Term -> bool) =>
    let isTrivialTerm := bundle_ in
    fun (t : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (_ : Literal) => true) (v_)
| Term_Variable v_ => (fun (nm : Name) => ((equality.equal) ((lists.length) (((strings.splitOn) ("."%string)) ((fun w_ => w_) (nm))))) ((1)%Z)) (v_)
| Term_Unit _ => true
| Term_Application v_ => (fun (app : Application) => let fun_ := (fun r_ => (application_function) (r_)) (app) in let arg := (fun r_ => (application_argument) (r_)) (app) in (fun x_ => match x_ with
| Term_Function v_ => (fun (f : Function) => (fun x_ => match x_ with
| Function_Elimination v_ => (fun (e : Elimination) => (fun x_ => match x_ with
| Elimination_Record v_ => (fun (_ : Projection) => (isTrivialTerm) (arg)) (v_)
| Elimination_Wrap v_ => (fun (_ : Name) => (isTrivialTerm) (arg)) (v_)
| _ => false
end) (e)) (v_)
| _ => false
end) (f)) (v_)
| _ => false
end) (fun_)) (v_)
| Term_Maybe v_ => (fun (opt : (option) (Term)) => (((maybes.maybe) (true)) (fun (inner : Term) => (isTrivialTerm) (inner))) (opt)) (v_)
| Term_Record v_ => (fun (rec : Record_) => (((lists.foldl) (fun (acc : bool) => fun (fld : Field) => ((logic.and) (acc)) ((isTrivialTerm) ((fun r_ => (field_term) (r_)) (fld))))) (true)) ((fun r_ => (record__fields) (r_)) (rec))) (v_)
| Term_Wrap v_ => (fun (wt : WrappedTerm) => (isTrivialTerm) ((fun r_ => (wrappedTerm_body) (r_)) (wt))) (v_)
| Term_TypeApplication v_ => (fun (ta : TypeApplicationTerm) => (isTrivialTerm) ((fun r_ => (typeApplicationTerm_body) (r_)) (ta))) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => (isTrivialTerm) ((fun r_ => (typeLambda_body) (r_)) (tl))) (v_)
| _ => false
end) ((deannotateTerm) (t))).

Definition isTrivialTerm : Term -> bool :=
  isTrivialTerm_bundle.
Definition isSerializableType : Type_ -> bool :=
  fun (typ : Type_) => let allVariants := (sets.fromList) (((lists.map) (hydra.reflect.typeVariant)) (((((foldOverType) ((TraversalOrder_Pre) (tt))) (fun (m : (list) (Type_)) => fun (t : Type_) => ((lists.cons) (t)) (m))) (nil)) (typ))) in (logic.not) (((sets.member) ((TypeVariant_Function) (tt))) (allVariants)).
Definition isSerializableByName : Context_ -> hydra.graph.Graph -> Name -> (sum) (Error) (bool) :=
  fun (cx : Context_) => fun (graph_ : hydra.graph.Graph) => fun (name : Name) => let variants := fun (typ : Type_) => ((lists.map) (hydra.reflect.typeVariant)) (((((foldOverType) ((TraversalOrder_Pre) (tt))) (fun (m : (list) (Type_)) => fun (t : Type_) => ((lists.cons) (t)) (m))) (nil)) (typ)) in ((eithers.map) (fun (deps : (list) ((prod) (Name) (Type_))) => let allVariants := (sets.fromList) ((lists.concat) (((lists.map) (variants)) ((maps.elems) (deps)))) in (logic.not) (((sets.member) ((TypeVariant_Function) (tt))) (allVariants)))) ((((((typeDependencies) (cx)) (graph_)) (false)) (equality.identity)) (name)).
Definition isSerializable : Context_ -> hydra.graph.Graph -> Binding -> (sum) (Error) (bool) :=
  fun (cx : Context_) => fun (graph_ : hydra.graph.Graph) => fun (el : Binding) => let variants := fun (typ : Type_) => ((lists.map) (hydra.reflect.typeVariant)) (((((foldOverType) ((TraversalOrder_Pre) (tt))) (fun (m : (list) (Type_)) => fun (t : Type_) => ((lists.cons) (t)) (m))) (nil)) (typ)) in ((eithers.map) (fun (deps : (list) ((prod) (Name) (Type_))) => let allVariants := (sets.fromList) ((lists.concat) (((lists.map) (variants)) ((maps.elems) (deps)))) in (logic.not) (((sets.member) ((TypeVariant_Function) (tt))) (allVariants)))) ((((((typeDependencies) (cx)) (graph_)) (false)) (equality.identity)) ((fun r_ => (binding_name) (r_)) (el))).
Definition isNominalType_bundle :=
  hydra_fix (fun (bundle_ : Type_ -> bool) =>
    let isNominalType := bundle_ in
    fun (typ : Type_) => (fun x_ => match x_ with
| Type__Record v_ => (fun (rt : (list) (FieldType)) => true) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => true) (v_)
| Type__Wrap v_ => (fun (wt : Type_) => true) (v_)
| Type__Forall v_ => (fun (fa : ForallType) => (isNominalType) ((fun r_ => (forallType_body) (r_)) (fa))) (v_)
| _ => false
end) ((deannotateType) (typ))).

Definition isNominalType : Type_ -> bool :=
  isNominalType_bundle.
Definition isEnumRowType : (list) (FieldType) -> bool :=
  fun (rt : (list) (FieldType)) => (((lists.foldl) (logic.and)) (true)) (((lists.map) (fun (f : FieldType) => (isUnitType) ((deannotateType) ((fun r_ => (fieldType_type) (r_)) (f))))) (rt)).
Definition isEnumType : Type_ -> bool :=
  fun (typ : Type_) => (fun x_ => match x_ with
| Type__Union v_ => (fun (rt : (list) (FieldType)) => (isEnumRowType) (rt)) (v_)
| _ => false
end) ((deannotateType) (typ)).
Definition isEncodedType_bundle :=
  hydra_fix (fun (bundle_ : Term -> bool) =>
    let isEncodedType := bundle_ in
    fun (t : Term) => (fun x_ => match x_ with
| Term_Application v_ => (fun (a : Application) => (isEncodedType) ((fun r_ => (application_function) (r_)) (a))) (v_)
| Term_Union v_ => (fun (i : Injection) => ((equality.equal) ("Type_"%string)) ((fun w_ => w_) ((fun r_ => (injection_typeName) (r_)) (i)))) (v_)
| _ => false
end) ((deannotateTerm) (t))).

Definition isEncodedType : Term -> bool :=
  isEncodedType_bundle.
Definition isEncodedTerm_bundle :=
  hydra_fix (fun (bundle_ : Term -> bool) =>
    let isEncodedTerm := bundle_ in
    fun (t : Term) => (fun x_ => match x_ with
| Term_Application v_ => (fun (a : Application) => (isEncodedTerm) ((fun r_ => (application_function) (r_)) (a))) (v_)
| Term_Union v_ => (fun (i : Injection) => ((equality.equal) ("Term"%string)) ((fun w_ => w_) ((fun r_ => (injection_typeName) (r_)) (i)))) (v_)
| _ => false
end) ((deannotateTerm) (t))).

Definition isEncodedTerm : Term -> bool :=
  isEncodedTerm_bundle.
Definition isComplexVariable : hydra.graph.Graph -> Name -> bool :=
  fun (tc : hydra.graph.Graph) => fun (name : Name) => let metaLookup := ((maps.lookup) (name)) ((fun r_ => (graph_metadata) (r_)) (tc)) in (((logic.ifElse) ((maybes.isJust) (metaLookup))) (true)) ((((logic.ifElse) (((sets.member) (name)) ((fun r_ => (graph_lambdaVariables) (r_)) (tc)))) (true)) (let typeLookup := ((maps.lookup) (name)) ((fun r_ => (graph_boundTypes) (r_)) (tc)) in (((maybes.maybe) (let primLookup := ((maps.lookup) (name)) ((fun r_ => (graph_primitives) (r_)) (tc)) in (((maybes.maybe) (true)) (fun (prim : Primitive) => ((equality.gt) ((typeSchemeArity) ((fun r_ => (primitive_type) (r_)) (prim)))) ((0)%Z))) (primLookup))) (fun (ts : TypeScheme) => ((equality.gt) ((typeSchemeArity) (ts))) ((0)%Z))) (typeLookup))).
Definition isComplexTerm_bundle :=
  hydra_fix (fun (bundle_ : hydra.graph.Graph -> Term -> bool) =>
    let isComplexTerm := bundle_ in
    fun (tc : hydra.graph.Graph) => fun (t : Term) => (fun x_ => match x_ with
| Term_Let v_ => (fun (_ : Let) => true) (v_)
| Term_TypeApplication v_ => (fun (_ : TypeApplicationTerm) => true) (v_)
| Term_TypeLambda v_ => (fun (_ : TypeLambda) => true) (v_)
| Term_Variable v_ => (fun (name : Name) => ((isComplexVariable) (tc)) (name)) (v_)
| _ => (((lists.foldl) (fun (b : bool) => fun (sub : Term) => ((logic.or) (b)) (((isComplexTerm) (tc)) (sub)))) (false)) ((subterms) (t))
end) (t)).

Definition isComplexTerm : hydra.graph.Graph -> Term -> bool :=
  isComplexTerm_bundle.
Definition isComplexBinding : hydra.graph.Graph -> Binding -> bool :=
  fun (tc : hydra.graph.Graph) => fun (b : Binding) => let term_ := (fun r_ => (binding_term) (r_)) (b) in let mts := (fun r_ => (binding_type) (r_)) (b) in (((maybes.cases) (mts)) (((isComplexTerm) (tc)) (term_))) (fun (ts : TypeScheme) => let isPolymorphic := (logic.not) ((lists.null) ((fun r_ => (typeScheme_variables) (r_)) (ts))) in let isNonNullary := ((equality.gt) ((typeArity) ((fun r_ => (typeScheme_type) (r_)) (ts)))) ((0)%Z) in let isComplex := ((isComplexTerm) (tc)) (term_) in ((logic.or) (((logic.or) (isPolymorphic)) (isNonNullary))) (isComplex)).

