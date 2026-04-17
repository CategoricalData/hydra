(* Dependency extraction, binding sort, and let normalization *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.coders hydra.core hydra.errors hydra.graph hydra.lexical hydra.lib.eithers hydra.lib.equality hydra.lib.lists hydra.lib.literals hydra.lib.logic hydra.lib.maps hydra.lib.math hydra.lib.maybes hydra.lib.pairs hydra.lib.sets hydra.lib.strings hydra.names hydra.packaging hydra.rewriting hydra.sorting hydra.strip hydra.variables.

Definition termDependencyNames : forall (_ : bool) , forall (_ : bool) , forall (_ : bool) , forall (_ : Term) , (list) (Name) := fun (binds : bool) => fun (withPrims : bool) => fun (withNoms : bool) => fun (term0 : Term) => let addNames := fun (names : (list) (Name)) => fun (term_ : Term) => let nominal := fun (name : Name) => (((logic.ifElse) (withNoms)) (((sets.insert) (name)) (names))) (names) in let prim := fun (name : Name) => (((logic.ifElse) (withPrims)) (((sets.insert) (name)) (names))) (names) in let var := fun (name : Name) => (((logic.ifElse) (binds)) (((sets.insert) (name)) (names))) (names) in (fun x_ => match x_ with
| Term_Cases v_ => (fun (caseStmt : CaseStatement) => (nominal) ((fun r_ => (caseStatement_typeName) (r_)) (caseStmt))) (v_)
| Term_Project v_ => (fun (proj : Projection) => (nominal) ((fun r_ => (projection_typeName) (r_)) (proj))) (v_)
| Term_Unwrap v_ => (fun (name : Name) => (nominal) (name)) (v_)
| Term_Record v_ => (fun (record : Record_) => (nominal) ((fun r_ => (record__typeName) (r_)) (record))) (v_)
| Term_Inject v_ => (fun (injection : Injection) => (nominal) ((fun r_ => (injection_typeName) (r_)) (injection))) (v_)
| Term_Variable v_ => (fun (name : Name) => (var) (name)) (v_)
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => (nominal) ((fun r_ => (wrappedTerm_typeName) (r_)) (wrappedTerm))) (v_)
| _ => names
end) (term_) in ((((foldOverTerm) ((TraversalOrder_Pre) (tt))) (addNames)) (sets.empty)) (term0).
Definition definitionsWithDependencies (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (Binding)) , (sum) (Error) ((list) (Binding)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (original : (list) (Binding)) => let depNames := fun (el : Binding) => (sets.toList) (((((termDependencyNames) (true)) (false)) (false)) ((fun r_ => (binding_term) (r_)) (el))) in let allDepNames := (lists.nub) (((lists.concat2) (((lists.map) (fun r_ => (binding_name) (r_))) (original))) ((lists.concat) (((lists.map) (depNames)) (original)))) in ((eithers.mapList) (fun (name : Name) => ((requireBinding) (graph_)) (name))) (allDepNames).
Arguments definitionsWithDependencies {t0}.
Definition flattenLetTerms : forall (_ : Term) , Term := fun (term_ : Term) => let flattenBodyLet := (hydra_fix) (fun flattenBodyLet => fun (bindings : (list) (Binding)) => fun (body : Term) => (fun x_ => match x_ with
| Term_Let v_ => (fun (innerLt : Let) => let innerBindings := (fun r_ => (let_bindings) (r_)) (innerLt) in let innerBody := (fun r_ => (let_body) (r_)) (innerLt) in ((flattenBodyLet) (((lists.concat2) (bindings)) (innerBindings))) (innerBody)) (v_)
| _ => (pair) (((lists.concat2) (nil)) (bindings)) (body)
end) (body)) in let rewriteBinding := (hydra_fix) (fun rewriteBinding => fun (binding : Binding) => let key0 := (fun r_ => (binding_name) (r_)) (binding) in let t := (fun r_ => (binding_type) (r_)) (binding) in let val0 := (fun r_ => (binding_term) (r_)) (binding) in (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => let ann := (fun r_ => (annotatedTerm_annotation) (r_)) (at_) in let val1 := (fun r_ => (annotatedTerm_body) (r_)) (at_) in let recursive := (rewriteBinding) ((Build_Binding) (key0) (val1) (t)) in let deps := (pairs.second) (recursive) in let innerBinding := (pairs.first) (recursive) in let val2 := (fun r_ => (binding_term) (r_)) (innerBinding) in (pair) ((Build_Binding) (key0) ((Term_Annotated) ((Build_AnnotatedTerm) (val2) (ann))) (t)) (deps)) (v_)
| Term_Let v_ => (fun (innerLet : Let) => let bindings1 := (fun r_ => (let_bindings) (r_)) (innerLet) in let body1 := (fun r_ => (let_body) (r_)) (innerLet) in let prefix := ((strings.cat2) ((fun w_ => w_) (key0))) ("_"%string) in let qualify := fun (n : Name) => ((strings.cat2) (prefix)) ((fun w_ => w_) (n)) in let toSubstPair := fun (b : Binding) => (pair) ((fun r_ => (binding_name) (r_)) (b)) ((qualify) ((fun r_ => (binding_name) (r_)) (b))) in let subst := (maps.fromList) (((lists.map) (toSubstPair)) (bindings1)) in let replaceVars := (substituteVariables) (subst) in let newBinding := fun (b : Binding) => (Build_Binding) ((qualify) ((fun r_ => (binding_name) (r_)) (b))) ((replaceVars) ((fun r_ => (binding_term) (r_)) (b))) ((fun r_ => (binding_type) (r_)) (b)) in let newBody := (replaceVars) (body1) in (pair) ((Build_Binding) (key0) (newBody) (t)) (((lists.map) (newBinding)) (bindings1))) (v_)
| _ => (pair) ((Build_Binding) (key0) (val0) (t)) (nil)
end) (val0)) in let flatten := fun recurse => fun term2 => let rewritten := (recurse) (term2) in (fun x_ => match x_ with
| Term_Let v_ => (fun (lt : Let) => let bindings := (fun r_ => (let_bindings) (r_)) (lt) in let body := (fun r_ => (let_body) (r_)) (lt) in let forResult := fun hr => ((lists.concat2) ((pairs.second) (hr))) ((lists.pure) ((pairs.first) (hr))) in let flattenedBindings := (lists.concat) (((lists.map) (fun (arg_ : Binding) => (forResult) ((rewriteBinding) (arg_)))) (bindings)) in let merged := ((flattenBodyLet) (flattenedBindings)) (body) in let newBindings := (pairs.first) (merged) in let newBody := (pairs.second) (merged) in (Term_Let) ((Build_Let) (newBindings) (newBody))) (v_)
| _ => rewritten
end) (rewritten) in ((rewriteTerm) (flatten)) (term_).
Definition inlineType_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : (list) ((prod) (Name) (Type_))) , forall (_ : Type_) , (sum) (Error) (Type_)) =>
    let inlineType := bundle_ in
    fun (schema : (list) ((prod) (Name) (Type_))) => fun (typ : Type_) => let f := fun recurse => fun typ2 => let afterRecurse := fun (tr : Type_) => (fun x_ => match x_ with
| Type__Variable v_ => (fun (v : Name) => (((maybes.maybe) (((inl) ((Error_Other) (((strings.cat2) ("No such type in schema: "%string)) ((fun w_ => w_) (v))))) : (sum) (Error) (Type_))) ((inlineType) (schema))) (((maps.lookup) (v)) (schema))) (v_)
| _ => ((inr) (tr)) : (sum) (Error) (Type_)
end) (tr) in ((eithers.bind) ((recurse) (typ2))) (fun (tr : Type_) => (afterRecurse) (tr)) in ((rewriteTypeM) (f)) (typ)).

Definition inlineType : forall (_ : (list) ((prod) (Name) (Type_))) , forall (_ : Type_) , (sum) (Error) (Type_) :=
  inlineType_bundle.
Definition isLambda_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , bool) =>
    let isLambda := bundle_ in
    fun (term_ : Term) => (fun x_ => match x_ with
| Term_Lambda v_ => (fun (_ : Lambda) => true) (v_)
| Term_Let v_ => (fun (lt : Let) => (isLambda) ((fun r_ => (let_body) (r_)) (lt))) (v_)
| _ => false
end) ((deannotateTerm) (term_))).

Definition isLambda : forall (_ : Term) , bool :=
  isLambda_bundle.
Definition liftLambdaAboveLet : forall (_ : Term) , Term := fun (term0 : Term) => let rewrite := (hydra_fix) (fun rewrite => fun (recurse : forall (_ : Term) , Term) => fun (term_ : Term) => let rewriteBinding := fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) (((rewrite) (recurse)) ((fun r_ => (binding_term) (r_)) (b))) ((fun r_ => (binding_type) (r_)) (b)) in let rewriteBindings := fun (bs : (list) (Binding)) => ((lists.map) (rewriteBinding)) (bs) in let digForLambdas := (hydra_fix) (fun digForLambdas => fun (original : Term) => fun (cons_ : forall (_ : Term) , Term) => fun (term2 : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (((digForLambdas) (original)) (fun (t : Term) => (Term_Annotated) ((Build_AnnotatedTerm) ((cons_) (t)) ((fun r_ => (annotatedTerm_annotation) (r_)) (at_))))) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| Term_Lambda v_ => (fun (l : Lambda) => (Term_Lambda) ((Build_Lambda) ((fun r_ => (lambda_parameter) (r_)) (l)) ((fun r_ => (lambda_domain) (r_)) (l)) ((((digForLambdas) ((cons_) ((fun r_ => (lambda_body) (r_)) (l)))) (fun (t : Term) => (cons_) (t))) ((fun r_ => (lambda_body) (r_)) (l))))) (v_)
| Term_Let v_ => (fun (l : Let) => (((digForLambdas) (original)) (fun (t : Term) => (cons_) ((Term_Let) ((Build_Let) ((rewriteBindings) ((fun r_ => (let_bindings) (r_)) (l))) (t))))) ((fun r_ => (let_body) (r_)) (l))) (v_)
| _ => (recurse) (original)
end) (term2)) in (fun x_ => match x_ with
| Term_Let v_ => (fun (l : Let) => (((digForLambdas) (term_)) (fun (t : Term) => (Term_Let) ((Build_Let) ((rewriteBindings) ((fun r_ => (let_bindings) (r_)) (l))) (t)))) ((fun r_ => (let_body) (r_)) (l))) (v_)
| _ => (recurse) (term_)
end) (term_)) in ((rewriteTerm) (rewrite)) (term0).
Definition pruneLet : forall (_ : Let) , Let := fun (l : Let) => let bindingMap := (maps.fromList) (((lists.map) (fun (b : Binding) => (pair) ((fun r_ => (binding_name) (r_)) (b)) ((fun r_ => (binding_term) (r_)) (b)))) ((fun r_ => (let_bindings) (r_)) (l))) in let rootName := "[[[root]]]"%string in let adj := fun (n : Name) => ((sets.intersection) ((sets.fromList) ((maps.keys) (bindingMap)))) ((freeVariablesInTerm) ((((logic.ifElse) (((equality.equal) (n)) (rootName))) ((fun r_ => (let_body) (r_)) (l))) ((maybes.fromJust) (((maps.lookup) (n)) (bindingMap))))) in let reachable := ((findReachableNodes) (adj)) (rootName) in let prunedBindings := ((lists.filter) (fun (b : Binding) => ((sets.member) ((fun r_ => (binding_name) (r_)) (b))) (reachable))) ((fun r_ => (let_bindings) (r_)) (l)) in (Build_Let) (prunedBindings) ((fun r_ => (let_body) (r_)) (l)).
Definition replaceTypedefs : forall (_ : (list) ((prod) (Name) (TypeScheme))) , forall (_ : Type_) , Type_ := fun (types : (list) ((prod) (Name) (TypeScheme))) => fun (typ0 : Type_) => let rewrite := (hydra_fix) (fun rewrite => fun (recurse : forall (_ : Type_) , Type_) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (Type__Annotated) ((Build_AnnotatedType) (((rewrite) (recurse)) ((fun r_ => (annotatedType_body) (r_)) (at_))) ((fun r_ => (annotatedType_annotation) (r_)) (at_)))) (v_)
| Type__Record v_ => (fun (_ : (list) (FieldType)) => typ) (v_)
| Type__Union v_ => (fun (_ : (list) (FieldType)) => typ) (v_)
| Type__Variable v_ => (fun (v : Name) => let forMono := fun (t : Type_) => (fun x_ => match x_ with
| Type__Record v_ => (fun (_ : (list) (FieldType)) => typ) (v_)
| Type__Union v_ => (fun (_ : (list) (FieldType)) => typ) (v_)
| Type__Wrap v_ => (fun (_ : Type_) => typ) (v_)
| _ => ((rewrite) (recurse)) (t)
end) (t) in let forTypeScheme := fun (ts : TypeScheme) => let t := (fun r_ => (typeScheme_type) (r_)) (ts) in (((logic.ifElse) ((lists.null) ((fun r_ => (typeScheme_variables) (r_)) (ts)))) ((forMono) (t))) (typ) in (((maybes.maybe) (typ)) (fun (ts : TypeScheme) => (forTypeScheme) (ts))) (((maps.lookup) (v)) (types))) (v_)
| Type__Wrap v_ => (fun (_ : Type_) => typ) (v_)
| _ => (recurse) (typ)
end) (typ)) in ((rewriteType) (rewrite)) (typ0).
Definition simplifyTerm_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , Term) =>
    let simplifyTerm := bundle_ in
    fun (term_ : Term) => let simplify := fun recurse => fun (term2 : Term) => let forRhs := fun (rhs : Term) => fun (var : Name) => fun (body : Term) => (fun x_ => match x_ with
| Term_Variable v_ => (fun (v : Name) => (simplifyTerm) ((((substituteVariable) (var)) (v)) (body))) (v_)
| _ => term2
end) ((deannotateTerm) (rhs)) in let forLhs := fun (lhs : Term) => fun (rhs : Term) => (fun x_ => match x_ with
| Term_Lambda v_ => (fun (l : Lambda) => let body := (fun r_ => (lambda_body) (r_)) (l) in let var := (fun r_ => (lambda_parameter) (r_)) (l) in (((logic.ifElse) (((sets.member) (var)) ((freeVariablesInTerm) (body)))) ((((forRhs) (rhs)) (var)) (body))) ((simplifyTerm) (body))) (v_)
| _ => term2
end) ((deannotateTerm) (lhs)) in let forTerm := fun (stripped : Term) => (fun x_ => match x_ with
| Term_Application v_ => (fun (app : Application) => let lhs := (fun r_ => (application_function) (r_)) (app) in let rhs := (fun r_ => (application_argument) (r_)) (app) in ((forLhs) (lhs)) (rhs)) (v_)
| _ => term2
end) (stripped) in let stripped := (deannotateTerm) (term2) in (recurse) ((forTerm) (stripped)) in ((rewriteTerm) (simplify)) (term_)).

Definition simplifyTerm : forall (_ : Term) , Term :=
  simplifyTerm_bundle.
Definition toShortNames : forall (_ : (list) (Name)) , (list) ((prod) (Name) (Name)) := fun (original : (list) (Name)) => let addName := fun (acc : (list) ((prod) (string) ((list) (Name)))) => fun (name : Name) => let local := (localNameOf) (name) in let group := ((maybes.fromMaybe) (sets.empty)) (((maps.lookup) (local)) (acc)) in (((maps.insert) (local)) (((sets.insert) (name)) (group))) (acc) in let groupNamesByLocal := fun (names : (list) (Name)) => (((lists.foldl) (addName)) (maps.empty)) (names) in let groups := (groupNamesByLocal) (original) in let renameGroup := fun localNames => let local := (pairs.first) (localNames) in let names := (pairs.second) (localNames) in let rangeFrom := (hydra_fix) (fun rangeFrom => fun (start : Z) => ((lists.cons) (start)) ((rangeFrom) (((math.add) (start)) ((1)%Z)))) in let rename := fun name => fun (i : Z) => (pair) (name) ((((logic.ifElse) (((equality.gt) (i)) ((1)%Z))) (((strings.cat2) (local)) ((literals.showInt32) (i)))) (local)) in (((lists.zipWith) (rename)) ((sets.toList) (names))) ((rangeFrom) ((1)%Z)) in (maps.fromList) ((lists.concat) (((lists.map) (renameGroup)) ((maps.toList) (groups)))).
Definition topologicalSortBindingMap : forall (_ : (list) ((prod) (Name) (Term))) , (list) ((list) ((prod) (Name) (Term))) := fun (bindingMap : (list) ((prod) (Name) (Term))) => let bindings := (maps.toList) (bindingMap) in let hasTypeAnnotation := (hydra_fix) (fun hasTypeAnnotation => fun (term_ : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (hasTypeAnnotation) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| _ => false
end) (term_)) in let keys := (sets.fromList) (((lists.map) (pairs.first)) (bindings)) in let depsOf := fun nameAndTerm => let name := (pairs.first) (nameAndTerm) in let term_ := (pairs.second) (nameAndTerm) in (pair) (name) ((((logic.ifElse) ((hasTypeAnnotation) (term_))) (nil)) ((sets.toList) (((sets.intersection) (keys)) ((freeVariablesInTerm) (term_))))) in let toPair := fun (name : Name) => (pair) (name) (((maybes.fromMaybe) ((Term_Literal) ((Literal_String) ("Impossible!"%string)))) (((maps.lookup) (name)) (bindingMap))) in ((lists.map) ((lists.map) (toPair))) ((topologicalSortComponents) (((lists.map) (depsOf)) (bindings))).
Definition topologicalSortBindings : forall (_ : (list) (Binding)) , (sum) ((list) ((list) (Name))) ((list) (Name)) := fun (els : (list) (Binding)) => let adjlist := fun (e : Binding) => (pair) ((fun r_ => (binding_name) (r_)) (e)) ((sets.toList) (((((termDependencyNames) (false)) (true)) (true)) ((fun r_ => (binding_term) (r_)) (e)))) in (topologicalSort) (((lists.map) (adjlist)) (els)).
Definition typeNamesInType (t0 : Type) : forall (_ : Type_) , (list) (t0) := fun (typ0 : Type_) => let addNames := fun (t1 : Type) => fun (t2 : Type) => fun (names : t1) => fun (typ : t2) => names in ((((foldOverType) ((TraversalOrder_Pre) (tt))) (((addNames) ((list) (t0))) (Type_))) (sets.empty)) (typ0).
Arguments typeNamesInType {t0}.
Definition typeDependencyNames : forall (_ : bool) , forall (_ : Type_) , (list) (Name) := fun (withSchema : bool) => fun (typ : Type_) => (((logic.ifElse) (withSchema)) (((sets.union) ((freeVariablesInType) (typ))) ((typeNamesInType) (typ)))) ((freeVariablesInType) (typ)).
Definition topologicalSortTypeDefinitions : forall (_ : (list) (TypeDefinition)) , (list) ((list) (TypeDefinition)) := fun (defs : (list) (TypeDefinition)) => let nameToDef := (maps.fromList) (((lists.map) (fun (d : TypeDefinition) => (pair) ((fun r_ => (typeDefinition_name) (r_)) (d)) (d))) (defs)) in let toPair := fun (def : TypeDefinition) => (pair) ((fun r_ => (typeDefinition_name) (r_)) (def)) ((sets.toList) (((typeDependencyNames) (false)) ((fun r_ => (typeScheme_type) (r_)) ((fun r_ => (typeDefinition_type) (r_)) (def))))) in let sorted := (topologicalSortComponents) (((lists.map) (toPair)) (defs)) in ((lists.map) (fun (names : (list) (Name)) => (maybes.cat) (((lists.map) (fun (n : Name) => ((maps.lookup) (n)) (nameToDef))) (names)))) (sorted).

