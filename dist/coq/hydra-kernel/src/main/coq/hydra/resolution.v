(* Type dereference, lookup, requirements, and instantiation *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.lists hydra.strip hydra.graph hydra.lib.maybes hydra.scoping hydra.lib.maps hydra.errors hydra.lib.eithers hydra.lib.strings hydra.show.core hydra.lib.equality hydra.lib.logic hydra.context hydra.names hydra.lib.pairs hydra.substitution hydra.lib.literals hydra.lib.math hydra.variables hydra.lexical hydra.decode.core.

Definition typeToTypeScheme : forall (_ : Type_) , TypeScheme := fun (t0 : Type_) => let helper := (hydra_fix) (fun helper => fun (vars : (list) (Name)) => fun (t : Type_) => (fun x_ => match x_ with
| Type__Forall v_ => (fun (ft : ForallType) => ((helper) (((lists.cons) ((fun r_ => (forallType_parameter) (r_)) (ft))) (vars))) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| _ => (Build_TypeScheme) ((lists.reverse) (vars)) (t) (None)
end) ((deannotateType) (t))) in ((helper) (nil)) (t0).
Definition resolveType : forall (_ : hydra.graph.Graph) , forall (_ : Type_) , (option) (Type_) := fun (graph_ : hydra.graph.Graph) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Variable v_ => (fun (name : Name) => (((maybes.maybe) (((maybes.map) (fun (ts : TypeScheme) => (typeSchemeToFType) (ts))) (((maps.lookup) (name)) ((fun r_ => (graph_boundTypes) (r_)) (graph_))))) (fun (ts : TypeScheme) => (Some) ((typeSchemeToFType) (ts)))) (((maps.lookup) (name)) ((fun r_ => (graph_schemaTypes) (r_)) (graph_)))) (v_)
| _ => (Some) (typ)
end) ((deannotateType) (typ)).
Definition requireType (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (Type_) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (name : Name) => (((maybes.maybe) ((((maybes.maybe) ((inl) ((Error_Resolution) ((ResolutionError_NoSuchBinding) ((Build_NoSuchBindingError) (name)))))) (fun (ts : TypeScheme) => (inr) ((typeSchemeToFType) (ts)))) (((maps.lookup) (name)) ((fun r_ => (graph_boundTypes) (r_)) (graph_))))) (fun (ts : TypeScheme) => (inr) ((typeSchemeToFType) (ts)))) (((maps.lookup) (name)) ((fun r_ => (graph_schemaTypes) (r_)) (graph_))).
Arguments requireType {t0}.
Definition requireRowType (t0 : Type) (t1 : Type) : forall (_ : t0) , forall (_ : string) , forall (_ : forall (_ : Type_) , (option) (t1)) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (t1) := fun (cx : t0) => fun (label : string) => fun (getter : forall (_ : Type_) , (option) (t1)) => fun (graph_ : hydra.graph.Graph) => fun (name : Name) => let rawType := (hydra_fix) (fun rawType => fun (t : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (rawType) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (rawType) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| _ => t
end) (t)) in ((eithers.bind) ((((requireType) (cx)) (graph_)) (name))) (fun (t : Type_) => (((maybes.maybe) ((inl) ((Error_Resolution) ((ResolutionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) (label)) (" type"%string)) (((strings.cat2) ((fun w_ => w_) (name))) (((strings.cat2) (": "%string)) ((hydra.show.core.type) (t))))))))) (fun (x : t1) => (inr) (x))) ((getter) ((rawType) (t)))).
Arguments requireRowType {t0} {t1}.
Definition requireUnionType (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) ((list) (FieldType)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (name : Name) => let toUnion := fun (t : Type_) => (fun x_ => match x_ with
| Type__Union v_ => (fun (rt : (list) (FieldType)) => (Some) (rt)) (v_)
| _ => None
end) (t) in (((((requireRowType) (cx)) ("union"%string)) (toUnion)) (graph_)) (name).
Arguments requireUnionType {t0}.
Definition requireUnionField (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , forall (_ : Name) , (sum) (Error) (Type_) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (tname : Name) => fun (fname : Name) => let withRowType := fun (rt : (list) (FieldType)) => let matches := ((lists.filter) (fun (ft : FieldType) => ((equality.equal) ((fun r_ => (fieldType_name) (r_)) (ft))) (fname))) (rt) in (((logic.ifElse) ((lists.null) (matches))) ((inl) ((Error_Resolution) ((ResolutionError_NoMatchingField) ((Build_NoMatchingFieldError) (fname)))))) ((inr) ((fun r_ => (fieldType_type) (r_)) ((lists.head) (matches)))) in ((eithers.bind) ((((requireUnionType) (cx)) (graph_)) (tname))) (withRowType).
Arguments requireUnionField {t0}.
Definition requireRecordType (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) ((list) (FieldType)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (name : Name) => let toRecord := fun (t : Type_) => (fun x_ => match x_ with
| Type__Record v_ => (fun (rt : (list) (FieldType)) => (Some) (rt)) (v_)
| _ => None
end) (t) in (((((requireRowType) (cx)) ("record type"%string)) (toRecord)) (graph_)) (name).
Arguments requireRecordType {t0}.
Definition nominalApplication : forall (_ : Name) , forall (_ : (list) (Type_)) , Type_ := fun (tname : Name) => fun (args : (list) (Type_)) => (((lists.foldl) (fun (t : Type_) => fun (a : Type_) => (Type__Application) ((Build_ApplicationType) (t) (a)))) ((Type__Variable) (tname))) (args).
Definition instantiateTypeScheme : forall (_ : Context_) , forall (_ : TypeScheme) , (prod) (TypeScheme) (Context_) := fun (cx : Context_) => fun (scheme : TypeScheme) => let oldVars := (fun r_ => (typeScheme_variables) (r_)) (scheme) in let result := ((freshNames) ((lists.length) (oldVars))) (cx) in let newVars := (pairs.first) (result) in let subst := (maps.fromList) (((lists.zip) (oldVars)) (((lists.map) (fun (x : Name) => (Type__Variable) (x))) (newVars))) in let nameSubst := (maps.fromList) (((lists.zip) (oldVars)) (newVars)) in let renamedConstraints := ((maybes.map) (fun (oldConstraints : (list) ((prod) (Name) (TypeVariableMetadata))) => (maps.fromList) (((lists.map) (fun (kv : (prod) (Name) (TypeVariableMetadata)) => (pair) (((maybes.fromMaybe) ((pairs.first) (kv))) (((maps.lookup) ((pairs.first) (kv))) (nameSubst))) ((pairs.second) (kv)))) ((maps.toList) (oldConstraints))))) ((fun r_ => (typeScheme_constraints) (r_)) (scheme)) in let cx2 := (pairs.second) (result) in (pair) ((Build_TypeScheme) (newVars) (((substInType) (subst)) ((fun r_ => (typeScheme_type) (r_)) (scheme))) (renamedConstraints)) (cx2).
Definition requireSchemaType : forall (_ : Context_) , forall (_ : (list) ((prod) (Name) (TypeScheme))) , forall (_ : Name) , (sum) (Error) ((prod) (TypeScheme) (Context_)) := fun (cx : Context_) => fun (types : (list) ((prod) (Name) (TypeScheme))) => fun (tname : Name) => (((maybes.maybe) ((inl) ((Error_Resolution) ((ResolutionError_NoSuchBinding) ((Build_NoSuchBindingError) (tname)))))) (fun (ts : TypeScheme) => (inr) (((instantiateTypeScheme) (cx)) ((deannotateTypeSchemeRecursive) (ts))))) (((maps.lookup) (tname)) (types)).
Definition instantiateType : forall (_ : Context_) , forall (_ : Type_) , (prod) (Type_) (Context_) := fun (cx : Context_) => fun (typ : Type_) => let result := ((instantiateTypeScheme) (cx)) ((typeToTypeScheme) (typ)) in (pair) ((typeSchemeToFType) ((pairs.first) (result))) ((pairs.second) (result)).
Definition fullyStripType_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , Type_) =>
    let fullyStripType := bundle_ in
    fun (typ : Type_) => (fun x_ => match x_ with
| Type__Forall v_ => (fun (ft : ForallType) => (fullyStripType) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| _ => typ
end) ((deannotateType) (typ))).

Definition fullyStripType : forall (_ : Type_) , Type_ :=
  fullyStripType_bundle.
Definition fullyStripAndNormalizeType : forall (_ : Type_) , Type_ := fun (typ : Type_) => let go := (hydra_fix) (fun go => fun (depth : Z) => fun (subst : (list) ((prod) (Name) (Name))) => fun (t : Type_) => (fun x_ => match x_ with
| Type__Forall v_ => (fun (ft : ForallType) => let oldVar := (fun r_ => (forallType_parameter) (r_)) (ft) in let newVar := ((strings.cat2) ("_"%string)) ((literals.showInt32) (depth)) in (((go) (((math.add) (depth)) ((1)%Z))) ((((maps.insert) (oldVar)) (newVar)) (subst))) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| _ => (pair) (subst) (t)
end) ((deannotateType) (t))) in let result := (((go) ((0)%Z)) (maps.empty)) (typ) in let subst := (pairs.first) (result) in let body := (pairs.second) (result) in ((substituteTypeVariables) (subst)) (body).
Definition findFieldType (t0 : Type) : forall (_ : t0) , forall (_ : Name) , forall (_ : (list) (FieldType)) , (sum) (Error) (Type_) := fun (cx : t0) => fun (fname : Name) => fun (fields : (list) (FieldType)) => let matchingFields := ((lists.filter) (fun (ft : FieldType) => ((equality.equal) ((fun w_ => w_) ((fun r_ => (fieldType_name) (r_)) (ft)))) ((fun w_ => w_) (fname)))) (fields) in (((logic.ifElse) ((lists.null) (matchingFields))) ((inl) ((Error_Resolution) ((ResolutionError_NoMatchingField) ((Build_NoMatchingFieldError) (fname)))))) ((((logic.ifElse) (((equality.equal) ((lists.length) (matchingFields))) ((1)%Z))) ((inr) ((fun r_ => (fieldType_type) (r_)) ((lists.head) (matchingFields))))) ((inl) ((Error_Extraction) ((ExtractionError_MultipleFields) ((Build_MultipleFieldsError) (fname)))))).
Arguments findFieldType {t0}.
Definition fieldTypes_bundle (t0 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Type_) , (sum) (Error) ((list) ((prod) (Name) (Type_)))) =>
    let fieldTypes := bundle_ in
    fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (t : Type_) => let toMap := fun (fields : (list) (FieldType)) => (maps.fromList) (((lists.map) (fun (ft : FieldType) => (pair) ((fun r_ => (fieldType_name) (r_)) (ft)) ((fun r_ => (fieldType_type) (r_)) (ft)))) (fields)) in (fun x_ => match x_ with
| Type__Forall v_ => (fun (ft : ForallType) => (((fieldTypes) (cx)) (graph_)) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => (inr) ((toMap) (rt))) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => (inr) ((toMap) (rt))) (v_)
| Type__Variable v_ => (fun (name : Name) => (((maybes.maybe) (((eithers.bind) (((requireBinding) (graph_)) (name))) (fun (el : Binding) => ((eithers.bind) ((((eithers.bimap) (fun (_e : DecodingError) => (Error_Resolution) ((ResolutionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("type"%string) ((fun w_ => w_) (_e)))))) (fun (_a : Type_) => _a)) (((hydra.decode.core.type) (graph_)) ((fun r_ => (binding_term) (r_)) (el))))) (fun (decodedType : Type_) => (((fieldTypes) (cx)) (graph_)) (decodedType))))) (fun (ts : TypeScheme) => (((fieldTypes) (cx)) (graph_)) ((fun r_ => (typeScheme_type) (r_)) (ts)))) (((maps.lookup) (name)) ((fun r_ => (graph_schemaTypes) (r_)) (graph_)))) (v_)
| _ => (inl) ((Error_Resolution) ((ResolutionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("record or union type"%string) ((hydra.show.core.type) (t)))))
end) ((deannotateType) (t))).
Arguments fieldTypes_bundle {t0}.

Definition fieldTypes (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Type_) , (sum) (Error) ((list) ((prod) (Name) (Type_))) :=
  fieldTypes_bundle.
Arguments fieldTypes {t0}.
Definition fieldTypeMap : forall (_ : (list) (FieldType)) , (list) ((prod) (Name) (Type_)) := fun (fields : (list) (FieldType)) => let toPair := fun (f : FieldType) => (pair) ((fun r_ => (fieldType_name) (r_)) (f)) ((fun r_ => (fieldType_type) (r_)) (f)) in (maps.fromList) (((lists.map) (toPair)) (fields)).
Definition fieldMap : forall (_ : (list) (Field)) , (list) ((prod) (Name) (Term)) := fun (fields : (list) (Field)) => let toPair := fun (f : Field) => (pair) ((fun r_ => (field_name) (r_)) (f)) ((fun r_ => (field_term) (r_)) (f)) in (maps.fromList) (((lists.map) (toPair)) (fields)).
Definition fTypeIsPolymorphic_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , bool) =>
    let fTypeIsPolymorphic := bundle_ in
    fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (fTypeIsPolymorphic) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => true) (v_)
| _ => false
end) (typ)).

Definition fTypeIsPolymorphic : forall (_ : Type_) , bool :=
  fTypeIsPolymorphic_bundle.
Definition dereferenceType (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) ((option) (Type_)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (name : Name) => let mel := ((lookupBinding) (graph_)) (name) in (((maybes.maybe) ((inr) (None))) (fun (el : Binding) => ((eithers.map) (maybes.pure)) ((((eithers.bimap) (fun (_e : DecodingError) => (Error_Resolution) ((ResolutionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("type"%string) ((fun w_ => w_) (_e)))))) (fun (_a : Type_) => _a)) (((hydra.decode.core.type) (graph_)) ((fun r_ => (binding_term) (r_)) (el)))))) (mel).
Arguments dereferenceType {t0}.

