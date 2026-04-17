(* A module for lexical operations over graphs. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.context hydra.core hydra.errors hydra.graph hydra.lib.eithers hydra.lib.equality hydra.lib.lists hydra.lib.literals hydra.lib.logic hydra.lib.maps hydra.lib.math hydra.lib.maybes hydra.lib.pairs hydra.lib.sets hydra.lib.strings hydra.show.core hydra.strip.

Definition buildGraph : forall (_ : (list) (Binding)) , forall (_ : (list) ((prod) (Name) ((option) (Term)))) , forall (_ : (list) ((prod) (Name) (Primitive))) , hydra.graph.Graph := fun (elements : (list) (Binding)) => fun (environment : (list) ((prod) (Name) ((option) (Term)))) => fun (primitives : (list) ((prod) (Name) (Primitive))) => let elementTerms := (maps.fromList) (((lists.map) (fun (b : Binding) => (pair) ((fun r_ => (binding_name) (r_)) (b)) ((fun r_ => (binding_term) (r_)) (b)))) (elements)) in let elementTypes := (maps.fromList) ((maybes.cat) (((lists.map) (fun (b : Binding) => ((maybes.map) (fun (ts : TypeScheme) => (pair) ((fun r_ => (binding_name) (r_)) (b)) (ts))) ((fun r_ => (binding_type) (r_)) (b)))) (elements))) in let letTerms := ((maps.map) (fun (mt : (option) (Term)) => (maybes.fromJust) (mt))) (((maps.filter) (fun (mt : (option) (Term)) => (maybes.isJust) (mt))) (environment)) in let mergedTerms := ((maps.union) (elementTerms)) (letTerms) in let filteredTerms := ((maps.filterWithKey) (fun (k : Name) => fun (_v : Term) => (logic.not) (((maps.member) (k)) (primitives)))) (mergedTerms) in let filteredTypes := ((maps.filterWithKey) (fun (k : Name) => fun (_v : TypeScheme) => (logic.not) (((maps.member) (k)) (primitives)))) (elementTypes) in (Build_Graph) (filteredTerms) (filteredTypes) (maps.empty) ((sets.fromList) ((maps.keys) (((maps.filter) (fun (mt : (option) (Term)) => (maybes.isNothing) (mt))) (environment)))) (maps.empty) (primitives) (maps.empty) (sets.empty).
Definition chooseUniqueName : forall (_ : (list) (Name)) , forall (_ : Name) , Name := fun (reserved : (list) (Name)) => fun (name : Name) => let tryName := (hydra_fix) (fun tryName => fun (index : Z) => let candidate := (((logic.ifElse) (((equality.equal) (index)) ((1)%Z))) (name)) (((strings.cat2) ((fun w_ => w_) (name))) ((literals.showInt32) (index))) in (((logic.ifElse) (((sets.member) (candidate)) (reserved))) ((tryName) (((math.add) (index)) ((1)%Z)))) (candidate)) in (tryName) ((1)%Z).
Definition dereferenceSchemaType_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Name) , forall (_ : (list) ((prod) (Name) (TypeScheme))) , (option) (TypeScheme)) =>
    let dereferenceSchemaType := bundle_ in
    fun (name : Name) => fun (types : (list) ((prod) (Name) (TypeScheme))) => let forType := (hydra_fix) (fun forType => fun (t : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (forType) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => ((maybes.map) (fun (ts : TypeScheme) => (Build_TypeScheme) (((lists.cons) ((fun r_ => (forallType_parameter) (r_)) (ft))) ((fun r_ => (typeScheme_variables) (r_)) (ts))) ((fun r_ => (typeScheme_type) (r_)) (ts)) ((fun r_ => (typeScheme_constraints) (r_)) (ts)))) ((forType) ((fun r_ => (forallType_body) (r_)) (ft)))) (v_)
| Type__Variable v_ => (fun (v : Name) => ((dereferenceSchemaType) (v)) (types)) (v_)
| _ => (Some) ((Build_TypeScheme) (nil) (t) ((None) : (option) ((list) ((prod) (Name) (TypeVariableMetadata)))))
end) (t)) in ((maybes.bind) (((maps.lookup) (name)) (types))) (fun (ts : TypeScheme) => ((maybes.map) (fun (ts2 : TypeScheme) => (Build_TypeScheme) (((lists.concat2) ((fun r_ => (typeScheme_variables) (r_)) (ts))) ((fun r_ => (typeScheme_variables) (r_)) (ts2))) ((fun r_ => (typeScheme_type) (r_)) (ts2)) ((fun r_ => (typeScheme_constraints) (r_)) (ts2)))) ((forType) ((fun r_ => (typeScheme_type) (r_)) (ts))))).

Definition dereferenceSchemaType : forall (_ : Name) , forall (_ : (list) ((prod) (Name) (TypeScheme))) , (option) (TypeScheme) :=
  dereferenceSchemaType_bundle.
Definition lookupBinding : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (option) (Binding) := fun (graph_ : hydra.graph.Graph) => fun (name : Name) => ((maybes.map) (fun (term_ : Term) => (Build_Binding) (name) (term_) (((maps.lookup) (name)) ((fun r_ => (graph_boundTypes) (r_)) (graph_))))) (((maps.lookup) (name)) ((fun r_ => (graph_boundTerms) (r_)) (graph_))).
Definition dereferenceVariable : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (Binding) := fun (graph_ : hydra.graph.Graph) => fun (name : Name) => (((maybes.maybe) (((inl) ((Error_Resolution) ((ResolutionError_NoSuchBinding) ((Build_NoSuchBindingError) (name))))) : (sum) (Error) (Binding))) (fun (right_ : Binding) => ((inr) (right_)) : (sum) (Error) (Binding))) (((lookupBinding) (graph_)) (name)).
Definition elementsToGraph : forall (_ : hydra.graph.Graph) , forall (_ : (list) ((prod) (Name) (TypeScheme))) , forall (_ : (list) (Binding)) , hydra.graph.Graph := fun (parent : hydra.graph.Graph) => fun (schemaTypes : (list) ((prod) (Name) (TypeScheme))) => fun (elements : (list) (Binding)) => let prims := (fun r_ => (graph_primitives) (r_)) (parent) in let g := (((buildGraph) (elements)) (maps.empty)) (prims) in (Build_Graph) ((fun r_ => (graph_boundTerms) (r_)) (g)) ((fun r_ => (graph_boundTypes) (r_)) (g)) ((fun r_ => (graph_classConstraints) (r_)) (g)) ((fun r_ => (graph_lambdaVariables) (r_)) (g)) ((fun r_ => (graph_metadata) (r_)) (g)) ((fun r_ => (graph_primitives) (r_)) (g)) (schemaTypes) ((fun r_ => (graph_typeVariables) (r_)) (g)).
Definition emptyContext : Context_ := (Build_Context_) (nil) (nil) (maps.empty).
Definition emptyGraph : hydra.graph.Graph := (Build_Graph) (maps.empty) (maps.empty) (maps.empty) (sets.empty) (maps.empty) (maps.empty) (maps.empty) (sets.empty).
Definition fieldsOf_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , (list) (FieldType)) =>
    let fieldsOf := bundle_ in
    fun (t : Type_) => let stripped := (deannotateType) (t) in (fun x_ => match x_ with
| Type__Forall v_ => (fun (forallType : ForallType) => (fieldsOf) ((fun r_ => (forallType_body) (r_)) (forallType))) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => rt) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => rt) (v_)
| _ => nil
end) (stripped)).

Definition fieldsOf : forall (_ : Type_) , (list) (FieldType) :=
  fieldsOf_bundle.
Definition getField (t0 : Type) (t1 : Type) : forall (_ : (list) ((prod) (Name) (t0))) , forall (_ : Name) , forall (_ : forall (_ : t0) , (sum) (Error) (t1)) , (sum) (Error) (t1) := fun (m : (list) ((prod) (Name) (t0))) => fun (fname : Name) => fun (decode : forall (_ : t0) , (sum) (Error) (t1)) => (((maybes.maybe) ((inl) ((Error_Resolution) ((ResolutionError_NoMatchingField) ((Build_NoMatchingFieldError) (fname)))))) (decode)) (((maps.lookup) (fname)) (m)).
Arguments getField {t0} {t1}.
Definition graphToBindings : forall (_ : hydra.graph.Graph) , (list) (Binding) := fun (g : hydra.graph.Graph) => ((lists.map) (fun (p : (prod) (Name) (Term)) => let name := (pairs.first) (p) in let term_ := (pairs.second) (p) in (Build_Binding) (name) (term_) (((maps.lookup) (name)) ((fun r_ => (graph_boundTypes) (r_)) (g))))) ((maps.toList) ((fun r_ => (graph_boundTerms) (r_)) (g))).
Definition graphWithPrimitives : forall (_ : (list) (Primitive)) , forall (_ : (list) (Primitive)) , hydra.graph.Graph := fun (builtIn : (list) (Primitive)) => fun (userProvided : (list) (Primitive)) => let toMap := fun (ps : (list) (Primitive)) => (maps.fromList) (((lists.map) (fun (p : Primitive) => (pair) ((fun r_ => (primitive_name) (r_)) (p)) (p))) (ps)) in let prims := ((maps.union) ((toMap) (userProvided))) ((toMap) (builtIn)) in (((buildGraph) (nil)) (maps.empty)) (prims).
Definition lookupPrimitive : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (option) (Primitive) := fun (graph_ : hydra.graph.Graph) => fun (name : Name) => ((maps.lookup) (name)) ((fun r_ => (graph_primitives) (r_)) (graph_)).
Definition lookupTerm : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (option) (Term) := fun (graph_ : hydra.graph.Graph) => fun (name : Name) => ((maps.lookup) (name)) ((fun r_ => (graph_boundTerms) (r_)) (graph_)).
Definition requireBinding : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (Binding) := fun (graph_ : hydra.graph.Graph) => fun (name : Name) => let showAll := false in let ellipsis := fun (strings : (list) (string)) => (((logic.ifElse) (((logic.and) (((equality.gt) ((lists.length) (strings))) ((3)%Z))) ((logic.not) (showAll)))) (((lists.concat2) (((lists.take) ((3)%Z)) (strings))) ((cons) ("..."%string) (nil)))) (strings) in let errMsg := ((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) ("no such element: "%string)) ((fun w_ => w_) (name)))) (". Available elements: {"%string))) (((strings.intercalate) (", "%string)) ((ellipsis) (((lists.map) (fun w_ => w_)) ((maps.keys) ((fun r_ => (graph_boundTerms) (r_)) (graph_)))))))) ("}"%string) in (((maybes.maybe) (((inl) ((Error_Resolution) ((ResolutionError_Other) (errMsg)))) : (sum) (Error) (Binding))) (fun (x : Binding) => ((inr) (x)) : (sum) (Error) (Binding))) (((lookupBinding) (graph_)) (name)).
Definition matchUnion_bundle (t0 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : hydra.graph.Graph) , forall (_ : Name) , forall (_ : (list) ((prod) (Name) (forall (_ : Term) , (sum) (Error) (t0)))) , forall (_ : Term) , (sum) (Error) (t0)) =>
    let matchUnion := bundle_ in
    fun (graph_ : hydra.graph.Graph) => fun (tname : Name) => fun (pairs : (list) ((prod) (Name) (forall (_ : Term) , (sum) (Error) (t0)))) => fun (term_ : Term) => let mapping := (maps.fromList) (pairs) in let stripped := (deannotateAndDetypeTerm) (term_) in (fun x_ => match x_ with
| Term_Variable v_ => (fun (name : Name) => ((eithers.bind) (((requireBinding) (graph_)) (name))) (fun (el : Binding) => ((((matchUnion) (graph_)) (tname)) (pairs)) ((fun r_ => (binding_term) (r_)) (el)))) (v_)
| Term_Inject v_ => (fun (injection : Injection) => let exp := let fname := (fun r_ => (field_name) (r_)) ((fun r_ => (injection_field) (r_)) (injection)) in let val := (fun r_ => (field_term) (r_)) ((fun r_ => (injection_field) (r_)) (injection)) in (((maybes.maybe) ((inl) ((Error_Resolution) ((ResolutionError_NoMatchingField) ((Build_NoMatchingFieldError) (fname)))))) (fun (f : forall (_ : Term) , (sum) (Error) (t0)) => (f) (val))) (((maps.lookup) (fname)) (mapping)) in (((logic.ifElse) (((equality.equal) ((fun w_ => w_) ((fun r_ => (injection_typeName) (r_)) (injection)))) ((fun w_ => w_) (tname)))) (exp)) ((inl) ((Error_Resolution) ((ResolutionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) ("injection for type "%string)) ((fun w_ => w_) (tname))) ((hydra.show.core.term) (term_))))))) (v_)
| _ => (inl) ((Error_Resolution) ((ResolutionError_UnexpectedShape) ((Build_UnexpectedShapeError) (((strings.cat2) ("injection for type "%string)) ((fun w_ => w_) (tname))) ((hydra.show.core.term) (stripped)))))
end) (stripped)).
Arguments matchUnion_bundle {t0}.

Definition matchUnion (t0 : Type) : forall (_ : hydra.graph.Graph) , forall (_ : Name) , forall (_ : (list) ((prod) (Name) (forall (_ : Term) , (sum) (Error) (t0)))) , forall (_ : Term) , (sum) (Error) (t0) :=
  matchUnion_bundle.
Arguments matchUnion {t0}.
Definition matchUnitField (t0 : Type) (t1 : Type) (t2 : Type) (t3 : Type) : forall (_ : t0) , forall (_ : t1) , (prod) (t0) (forall (_ : t2) , (sum) (t3) (t1)) := fun (fname : t0) => fun (x : t1) => (pair) (fname) (fun (ignored : t2) => (inr) (x)).
Arguments matchUnitField {t0} {t1} {t2} {t3}.
Definition matchEnum (t0 : Type) : forall (_ : hydra.graph.Graph) , forall (_ : Name) , forall (_ : (list) ((prod) (Name) (t0))) , forall (_ : Term) , (sum) (Error) (t0) := fun (graph_ : hydra.graph.Graph) => fun (tname : Name) => fun (pairs : (list) ((prod) (Name) (t0))) => (((matchUnion) (graph_)) (tname)) (((lists.map) (fun (pair_ : (prod) (Name) (t0)) => ((matchUnitField) ((pairs.first) (pair_))) ((pairs.second) (pair_)))) (pairs)).
Arguments matchEnum {t0}.
Definition matchRecord (t0 : Type) (t1 : Type) : forall (_ : t0) , forall (_ : forall (_ : (list) ((prod) (Name) (Term))) , (sum) (Error) (t1)) , forall (_ : Term) , (sum) (Error) (t1) := fun (graph_ : t0) => fun (decode : forall (_ : (list) ((prod) (Name) (Term))) , (sum) (Error) (t1)) => fun (term_ : Term) => let stripped := (deannotateAndDetypeTerm) (term_) in (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => (decode) ((maps.fromList) (((lists.map) (fun (field : Field) => (pair) ((fun r_ => (field_name) (r_)) (field)) ((fun r_ => (field_term) (r_)) (field)))) ((fun r_ => (record__fields) (r_)) (record))))) (v_)
| _ => (inl) ((Error_Resolution) ((ResolutionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("record"%string) ((hydra.show.core.term) (term_)))))
end) (stripped).
Arguments matchRecord {t0} {t1}.
Definition requirePrimitive : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (Primitive) := fun (graph_ : hydra.graph.Graph) => fun (name : Name) => (((maybes.maybe) (((inl) ((Error_Resolution) ((ResolutionError_NoSuchPrimitive) ((Build_NoSuchPrimitiveError) (name))))) : (sum) (Error) (Primitive))) (fun (x : Primitive) => ((inr) (x)) : (sum) (Error) (Primitive))) (((lookupPrimitive) (graph_)) (name)).
Definition requirePrimitiveType : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (TypeScheme) := fun (tx : hydra.graph.Graph) => fun (name : Name) => let mts := ((maybes.map) (fun (_p : Primitive) => (fun r_ => (primitive_type) (r_)) (_p))) (((maps.lookup) (name)) ((fun r_ => (graph_primitives) (r_)) (tx))) in (((maybes.maybe) (((inl) ((Error_Resolution) ((ResolutionError_NoSuchPrimitive) ((Build_NoSuchPrimitiveError) (name))))) : (sum) (Error) (TypeScheme))) (fun (ts : TypeScheme) => ((inr) (ts)) : (sum) (Error) (TypeScheme))) (mts).
Definition resolveTerm_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (option) (Term)) =>
    let resolveTerm := bundle_ in
    fun (graph_ : hydra.graph.Graph) => fun (name : Name) => let recurse := fun (term_ : Term) => let stripped := (deannotateTerm) (term_) in (fun x_ => match x_ with
| Term_Variable v_ => (fun (name' : Name) => ((resolveTerm) (graph_)) (name')) (v_)
| _ => (Some) (term_)
end) (stripped) in (((maybes.maybe) ((None) : (option) (Term))) (recurse)) (((lookupTerm) (graph_)) (name))).

Definition resolveTerm : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (option) (Term) :=
  resolveTerm_bundle.
Definition requireTerm : forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (Term) := fun (graph_ : hydra.graph.Graph) => fun (name : Name) => (((maybes.maybe) (((inl) ((Error_Resolution) ((ResolutionError_NoSuchBinding) ((Build_NoSuchBindingError) (name))))) : (sum) (Error) (Term))) (fun (x : Term) => ((inr) (x)) : (sum) (Error) (Term))) (((resolveTerm) (graph_)) (name)).
Definition stripAndDereferenceTerm_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term)) =>
    let stripAndDereferenceTerm := bundle_ in
    fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => let stripped := (deannotateAndDetypeTerm) (term_) in (fun x_ => match x_ with
| Term_Variable v_ => (fun (v : Name) => ((eithers.bind) (((requireTerm) (graph_)) (v))) (fun (t : Term) => ((stripAndDereferenceTerm) (graph_)) (t))) (v_)
| _ => ((inr) (stripped)) : (sum) (Error) (Term)
end) (stripped)).

Definition stripAndDereferenceTerm : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term) :=
  stripAndDereferenceTerm_bundle.
Definition stripAndDereferenceTermEither_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term)) =>
    let stripAndDereferenceTermEither := bundle_ in
    fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => let stripped := (deannotateAndDetypeTerm) (term_) in (fun x_ => match x_ with
| Term_Variable v_ => (fun (v : Name) => (((eithers.either) (fun (left_ : Error) => ((inl) (left_)) : (sum) (Error) (Term))) (fun (binding : Binding) => ((stripAndDereferenceTermEither) (graph_)) ((fun r_ => (binding_term) (r_)) (binding)))) (((dereferenceVariable) (graph_)) (v))) (v_)
| _ => ((inr) (stripped)) : (sum) (Error) (Term)
end) (stripped)).

Definition stripAndDereferenceTermEither : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term) :=
  stripAndDereferenceTermEither_bundle.

