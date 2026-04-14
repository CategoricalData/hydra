(* Variable substitution in type and term expressions. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.typing hydra.core hydra.lib.sets hydra.lib.lists hydra.lib.maps hydra.lib.logic hydra.lib.maybes hydra.rewriting hydra.lib.pairs hydra.variables hydra.graph.

Definition substituteInTerm_bundle :=
  hydra_fix (fun (bundle_ : TermSubst -> Term -> Term) =>
    let substituteInTerm := bundle_ in
    fun (subst : TermSubst) => fun (term0 : Term) => let s := (fun w_ => w_) (subst) in let rewrite := fun (recurse : Term -> Term) => fun (term_ : Term) => let withLet := fun (lt : Let) => let bindings := (fun r_ => (let_bindings) (r_)) (lt) in let names := (sets.fromList) (((lists.map) (fun r_ => (binding_name) (r_))) (bindings)) in let subst2 := ((maps.filterWithKey) (fun (k : Name) => fun (v : Term) => (logic.not) (((sets.member) (k)) (names)))) (s) in let rewriteBinding := fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) (((substituteInTerm) (subst2)) ((fun r_ => (binding_term) (r_)) (b))) ((fun r_ => (binding_type) (r_)) (b)) in (Term_Let) ((Build_Let) (((lists.map) (rewriteBinding)) (bindings)) (((substituteInTerm) (subst2)) ((fun r_ => (let_body) (r_)) (lt)))) in let withLambda := fun (l : Lambda) => let v := (fun r_ => (lambda_parameter) (r_)) (l) in let subst2 := ((maps.delete) (v)) (s) in (Term_Lambda) ((Build_Lambda) (v) ((fun r_ => (lambda_domain) (r_)) (l)) (((substituteInTerm) (subst2)) ((fun r_ => (lambda_body) (r_)) (l)))) in (fun x_ => match x_ with
| Term_Lambda v_ => (fun (l : Lambda) => (withLambda) (l)) (v_)
| Term_Let v_ => (fun (l : Let) => (withLet) (l)) (v_)
| Term_Variable v_ => (fun (name : Name) => (((maybes.maybe) ((recurse) (term_))) (fun (sterm : Term) => sterm)) (((maps.lookup) (name)) (s))) (v_)
| _ => (recurse) (term_)
end) (term_) in ((rewriteTerm) (rewrite)) (term0)).

Definition substituteInTerm : TermSubst -> Term -> Term :=
  substituteInTerm_bundle.
Definition substituteInBinding : TermSubst -> Binding -> Binding :=
  fun (subst : TermSubst) => fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) (((substituteInTerm) (subst)) ((fun r_ => (binding_term) (r_)) (b))) ((fun r_ => (binding_type) (r_)) (b)).
Definition substInType_substInTypeNonEmpty_bundle :=
  hydra_fix (fun (bundle_ : prod (TypeSubst -> Type_ -> Type_) (TypeSubst -> Type_ -> Type_)) =>
    let substInType := (fst bundle_) in
    let substInTypeNonEmpty := (snd bundle_) in
    (pair (fun (subst : TypeSubst) => fun (typ0 : Type_) => (((logic.ifElse) ((maps.null) ((fun w_ => w_) (subst)))) (typ0)) (((substInTypeNonEmpty) (subst)) (typ0))) (fun (subst : TypeSubst) => fun (typ0 : Type_) => let removeVar := fun (v : Name) => ((maps.delete) (v)) ((fun w_ => w_) (subst)) in let rewrite := fun (recurse : Type_ -> Type_) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Forall v_ => (fun (lt : ForallType) => (((maybes.maybe) ((recurse) (typ))) (fun (styp : Type_) => (Type__Forall) ((Build_ForallType) ((fun r_ => (forallType_parameter) (r_)) (lt)) (((substInType) ((removeVar) ((fun r_ => (forallType_parameter) (r_)) (lt)))) ((fun r_ => (forallType_body) (r_)) (lt)))))) (((maps.lookup) ((fun r_ => (forallType_parameter) (r_)) (lt))) ((fun w_ => w_) (subst)))) (v_)
| Type__Variable v_ => (fun (v : Name) => (((maybes.maybe) (typ)) (fun (styp : Type_) => styp)) (((maps.lookup) (v)) ((fun w_ => w_) (subst)))) (v_)
| _ => (recurse) (typ)
end) (typ) in ((rewriteType) (rewrite)) (typ0)))).

Definition substInType : TypeSubst -> Type_ -> Type_ :=
  (fst substInType_substInTypeNonEmpty_bundle).
Definition substInTypeNonEmpty : TypeSubst -> Type_ -> Type_ :=
  (snd substInType_substInTypeNonEmpty_bundle).
Definition substituteInConstraint : TypeSubst -> TypeConstraint -> TypeConstraint :=
  fun (subst : TypeSubst) => fun (c : TypeConstraint) => (Build_TypeConstraint) (((substInType) (subst)) ((fun r_ => (typeConstraint_left) (r_)) (c))) (((substInType) (subst)) ((fun r_ => (typeConstraint_right) (r_)) (c))) ((fun r_ => (typeConstraint_comment) (r_)) (c)).
Definition substituteInConstraints : TypeSubst -> (list) (TypeConstraint) -> (list) (TypeConstraint) :=
  fun (subst : TypeSubst) => fun (cs : (list) (TypeConstraint)) => ((lists.map) ((substituteInConstraint) (subst))) (cs).
Definition substInClassConstraints : TypeSubst -> (list) ((prod) (Name) (TypeVariableMetadata)) -> (list) ((prod) (Name) (TypeVariableMetadata)) :=
  fun (subst : TypeSubst) => fun (constraints : (list) ((prod) (Name) (TypeVariableMetadata))) => let substMap := (fun w_ => w_) (subst) in let insertOrMerge := fun varName => fun (metadata : TypeVariableMetadata) => fun acc => (((maybes.maybe) ((((maps.insert) (varName)) (metadata)) (acc))) (fun (existing : TypeVariableMetadata) => let merged := (Build_TypeVariableMetadata) (((sets.union) ((fun r_ => (typeVariableMetadata_classes) (r_)) (existing))) ((fun r_ => (typeVariableMetadata_classes) (r_)) (metadata))) in (((maps.insert) (varName)) (merged)) (acc))) (((maps.lookup) (varName)) (acc)) in (((lists.foldl) (fun (acc : (list) ((prod) (Name) (TypeVariableMetadata))) => fun (pair_ : (prod) (Name) (TypeVariableMetadata)) => let varName := (pairs.first) (pair_) in let metadata := (pairs.second) (pair_) in (((maybes.maybe) ((((insertOrMerge) (varName)) (metadata)) (acc))) (fun (targetType : Type_) => let freeVars := (sets.toList) ((freeVariablesInType) (targetType)) in (((lists.foldl) (fun (acc2 : (list) ((prod) (Name) (TypeVariableMetadata))) => fun (freeVar : Name) => (((insertOrMerge) (freeVar)) (metadata)) (acc2))) (acc)) (freeVars))) (((maps.lookup) (varName)) (substMap)))) (maps.empty)) ((maps.toList) (constraints)).
Definition substInTypeScheme : TypeSubst -> TypeScheme -> TypeScheme :=
  fun (subst : TypeSubst) => fun (ts : TypeScheme) => (Build_TypeScheme) ((fun r_ => (typeScheme_variables) (r_)) (ts)) (((substInType) (subst)) ((fun r_ => (typeScheme_type) (r_)) (ts))) (((maybes.map) ((substInClassConstraints) (subst))) ((fun r_ => (typeScheme_constraints) (r_)) (ts))).
Definition substInContext : TypeSubst -> hydra.graph.Graph -> hydra.graph.Graph :=
  fun (subst : TypeSubst) => fun (cx : hydra.graph.Graph) => let newClassConstraints := ((substInClassConstraints) (subst)) ((fun r_ => (graph_classConstraints) (r_)) (cx)) in let newBoundTypes := ((maps.map) ((substInTypeScheme) (subst))) ((fun r_ => (graph_boundTypes) (r_)) (cx)) in let cx2 := (Build_Graph) ((fun r_ => (graph_boundTerms) (r_)) (cx)) (newBoundTypes) ((fun r_ => (graph_classConstraints) (r_)) (cx)) ((fun r_ => (graph_lambdaVariables) (r_)) (cx)) ((fun r_ => (graph_metadata) (r_)) (cx)) ((fun r_ => (graph_primitives) (r_)) (cx)) ((fun r_ => (graph_schemaTypes) (r_)) (cx)) ((fun r_ => (graph_typeVariables) (r_)) (cx)) in (Build_Graph) ((fun r_ => (graph_boundTerms) (r_)) (cx2)) ((fun r_ => (graph_boundTypes) (r_)) (cx2)) (newClassConstraints) ((fun r_ => (graph_lambdaVariables) (r_)) (cx2)) ((fun r_ => (graph_metadata) (r_)) (cx2)) ((fun r_ => (graph_primitives) (r_)) (cx2)) ((fun r_ => (graph_schemaTypes) (r_)) (cx2)) ((fun r_ => (graph_typeVariables) (r_)) (cx2)).
Definition substTypesInTerm_bundle :=
  hydra_fix (fun (bundle_ : TypeSubst -> Term -> Term) =>
    let substTypesInTerm := bundle_ in
    fun (subst : TypeSubst) => fun (term0 : Term) => let rewrite := fun (recurse : Term -> Term) => fun (term_ : Term) => let forTypeLambda := fun (ta : TypeLambda) => let param := (fun r_ => (typeLambda_parameter) (r_)) (ta) in let subst2 := ((maps.delete) (param)) ((fun w_ => w_) (subst)) in (Term_TypeLambda) ((Build_TypeLambda) (param) (((substTypesInTerm) (subst2)) ((fun r_ => (typeLambda_body) (r_)) (ta)))) in let forTypeApplication := fun (tt : TypeApplicationTerm) => (Term_TypeApplication) ((Build_TypeApplicationTerm) (((substTypesInTerm) (subst)) ((fun r_ => (typeApplicationTerm_body) (r_)) (tt))) (((substInType) (subst)) ((fun r_ => (typeApplicationTerm_type) (r_)) (tt)))) in let forLet := fun (l : Let) => let rewriteBinding := fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) (((substTypesInTerm) (subst)) ((fun r_ => (binding_term) (r_)) (b))) (((maybes.map) ((substInTypeScheme) (subst))) ((fun r_ => (binding_type) (r_)) (b))) in (Term_Let) ((Build_Let) (((lists.map) (rewriteBinding)) ((fun r_ => (let_bindings) (r_)) (l))) (((substTypesInTerm) (subst)) ((fun r_ => (let_body) (r_)) (l)))) in let forLambda := fun (l : Lambda) => (Term_Lambda) ((Build_Lambda) ((fun r_ => (lambda_parameter) (r_)) (l)) (((maybes.map) ((substInType) (subst))) ((fun r_ => (lambda_domain) (r_)) (l))) (((substTypesInTerm) (subst)) ((fun r_ => (lambda_body) (r_)) (l)))) in let dflt := (recurse) (term_) in (fun x_ => match x_ with
| Term_Lambda v_ => (fun (l : Lambda) => (forLambda) (l)) (v_)
| Term_Let v_ => (fun (l : Let) => (forLet) (l)) (v_)
| Term_TypeApplication v_ => (fun (ta : TypeApplicationTerm) => (forTypeApplication) (ta)) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => (forTypeLambda) (tl)) (v_)
| _ => dflt
end) (term_) in ((rewriteTerm) (rewrite)) (term0)).

Definition substTypesInTerm : TypeSubst -> Term -> Term :=
  substTypesInTerm_bundle.
Definition singletonTypeSubst : Name -> Type_ -> TypeSubst :=
  fun (v : Name) => fun (t : Type_) => ((maps.singleton) (v)) (t).
Definition idTypeSubst : TypeSubst :=
  maps.empty.
Definition composeTypeSubstNonEmpty : TypeSubst -> TypeSubst -> TypeSubst :=
  fun (s1 : TypeSubst) => fun (s2 : TypeSubst) => let isExtra := fun (k : Name) => fun v => (maybes.isNothing) (((maps.lookup) (k)) ((fun w_ => w_) (s1))) in let withExtra := ((maps.filterWithKey) (isExtra)) ((fun w_ => w_) (s2)) in ((maps.union) (withExtra)) (((maps.map) ((substInType) (s2))) ((fun w_ => w_) (s1))).
Definition composeTypeSubst : TypeSubst -> TypeSubst -> TypeSubst :=
  fun (s1 : TypeSubst) => fun (s2 : TypeSubst) => (((logic.ifElse) ((maps.null) ((fun w_ => w_) (s1)))) (s2)) ((((logic.ifElse) ((maps.null) ((fun w_ => w_) (s2)))) (s1)) (((composeTypeSubstNonEmpty) (s1)) (s2))).
Definition composeTypeSubstList : (list) (TypeSubst) -> TypeSubst :=
  ((lists.foldl) (composeTypeSubst)) (idTypeSubst).

