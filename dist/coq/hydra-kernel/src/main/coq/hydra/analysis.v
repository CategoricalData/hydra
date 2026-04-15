(* Module dependency namespace analysis *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.annotations hydra.checking hydra.coders hydra.constants hydra.context hydra.core hydra.decode.core hydra.dependencies hydra.encode.core hydra.errors hydra.graph hydra.lib.eithers hydra.lib.equality hydra.lib.lists hydra.lib.logic hydra.lib.maps hydra.lib.maybes hydra.lib.pairs hydra.lib.sets hydra.names hydra.packaging hydra.predicates hydra.rewriting hydra.scoping hydra.strip hydra.typing hydra.variables.

Definition addNamesToNamespaces (t0 : Type) : forall (_ : forall (_ : Namespace) , t0) , forall (_ : (list) (Name)) , forall (_ : (Namespaces) (t0)) , (Namespaces) (t0) := fun (encodeNamespace : forall (_ : Namespace) , t0) => fun (names : (list) (Name)) => fun (ns0 : (Namespaces) (t0)) => let nss := (sets.fromList) ((maybes.cat) (((lists.map) (namespaceOf)) ((sets.toList) (names)))) in let toPair := fun (ns : Namespace) => (pair) (ns) ((encodeNamespace) (ns)) in (Build_Namespaces) ((fun r_ => (namespaces_focus) (r_)) (ns0)) (((maps.union) ((fun r_ => (namespaces_mapping) (r_)) (ns0))) ((maps.fromList) (((lists.map) (toPair)) ((sets.toList) (nss))))).
Arguments addNamesToNamespaces {t0}.
Definition analyzeFunctionTermWith_finish (t0 : Type) (t1 : Type) : forall (_ : Context_) , forall (_ : forall (_ : t0) , hydra.graph.Graph) , forall (_ : t0) , forall (_ : (list) (Name)) , forall (_ : (list) (Name)) , forall (_ : (list) (Binding)) , forall (_ : (list) (Type_)) , forall (_ : (list) (Type_)) , forall (_ : Term) , (sum) (t1) ((FunctionStructure) (t0)) := fun (cx : Context_) => fun (getTC : forall (_ : t0) , hydra.graph.Graph) => fun (fEnv : t0) => fun (tparams : (list) (Name)) => fun (args : (list) (Name)) => fun (bindings : (list) (Binding)) => fun (doms : (list) (Type_)) => fun (tapps : (list) (Type_)) => fun (body : Term) => let bodyWithTapps := (((lists.foldl) (fun (trm : Term) => fun (typ : Type_) => (Term_TypeApplication) ((Build_TypeApplicationTerm) (trm) (typ)))) (body)) (tapps) in let mcod := (((eithers.either) (fun (_ : Error) => None)) (fun (c : Type_) => (Some) (c))) ((((typeOfTerm) (cx)) ((getTC) (fEnv))) (bodyWithTapps)) in (inr) ((Build_FunctionStructure) ((lists.reverse) (tparams)) ((lists.reverse) (args)) (bindings) (bodyWithTapps) ((lists.reverse) (doms)) (mcod) (fEnv)).
Arguments analyzeFunctionTermWith_finish {t0} {t1}.
Definition analyzeFunctionTermWith_gather_bundle (t0 : Type) (t1 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : Context_) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Binding) , (option) (Term)) , forall (_ : forall (_ : t0) , hydra.graph.Graph) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : t0) , t0) , forall (_ : bool) , forall (_ : t0) , forall (_ : (list) (Name)) , forall (_ : (list) (Name)) , forall (_ : (list) (Binding)) , forall (_ : (list) (Type_)) , forall (_ : (list) (Type_)) , forall (_ : Term) , (sum) (t1) ((FunctionStructure) (t0))) =>
    let analyzeFunctionTermWith_gather := bundle_ in
    fun (cx : Context_) => fun (forBinding : forall (_ : hydra.graph.Graph) , forall (_ : Binding) , (option) (Term)) => fun (getTC : forall (_ : t0) , hydra.graph.Graph) => fun (setTC : forall (_ : hydra.graph.Graph) , forall (_ : t0) , t0) => fun (argMode : bool) => fun (gEnv : t0) => fun (tparams : (list) (Name)) => fun (args : (list) (Name)) => fun (bindings : (list) (Binding)) => fun (doms : (list) (Type_)) => fun (tapps : (list) (Type_)) => fun (t : Term) => (fun x_ => match x_ with
| Term_Lambda v_ => (fun (lam : Lambda) => (((logic.ifElse) (argMode)) (let body := (fun r_ => (lambda_body) (r_)) (lam) in let dom := (((maybes.maybe) ((Type__Variable) ("_"%string))) (fun (x_ : Type_) => x_)) ((fun r_ => (lambda_domain) (r_)) (lam)) in let newEnv := ((setTC) (((extendGraphForLambda) ((getTC) (gEnv))) (lam))) (gEnv) in let v := (fun r_ => (lambda_parameter) (r_)) (lam) in ((((((((((((analyzeFunctionTermWith_gather) (cx)) (forBinding)) (getTC)) (setTC)) (argMode)) (newEnv)) (tparams)) (((lists.cons) (v)) (args))) (bindings)) (((lists.cons) (dom)) (doms))) (tapps)) (body))) ((((((((((analyzeFunctionTermWith_finish) (cx)) (getTC)) (gEnv)) (tparams)) (args)) (bindings)) (doms)) (tapps)) (t))) (v_)
| Term_Let v_ => (fun (lt : Let) => let body := (fun r_ => (let_body) (r_)) (lt) in let newBindings := (fun r_ => (let_bindings) (r_)) (lt) in let newEnv := ((setTC) ((((extendGraphForLet) (forBinding)) ((getTC) (gEnv))) (lt))) (gEnv) in ((((((((((((analyzeFunctionTermWith_gather) (cx)) (forBinding)) (getTC)) (setTC)) (false)) (newEnv)) (tparams)) (args)) (((lists.concat2) (bindings)) (newBindings))) (doms)) (tapps)) (body)) (v_)
| Term_TypeApplication v_ => (fun (ta : TypeApplicationTerm) => let taBody := (fun r_ => (typeApplicationTerm_body) (r_)) (ta) in let typ := (fun r_ => (typeApplicationTerm_type) (r_)) (ta) in ((((((((((((analyzeFunctionTermWith_gather) (cx)) (forBinding)) (getTC)) (setTC)) (argMode)) (gEnv)) (tparams)) (args)) (bindings)) (doms)) (((lists.cons) (typ)) (tapps))) (taBody)) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => let newEnv := ((setTC) (((extendGraphForTypeLambda) ((getTC) (gEnv))) (tl))) (gEnv) in let tlBody := (fun r_ => (typeLambda_body) (r_)) (tl) in let tvar := (fun r_ => (typeLambda_parameter) (r_)) (tl) in ((((((((((((analyzeFunctionTermWith_gather) (cx)) (forBinding)) (getTC)) (setTC)) (argMode)) (newEnv)) (((lists.cons) (tvar)) (tparams))) (args)) (bindings)) (doms)) (tapps)) (tlBody)) (v_)
| _ => (((((((((analyzeFunctionTermWith_finish) (cx)) (getTC)) (gEnv)) (tparams)) (args)) (bindings)) (doms)) (tapps)) (t)
end) ((deannotateTerm) (t))).
Arguments analyzeFunctionTermWith_gather_bundle {t0} {t1}.

Definition analyzeFunctionTermWith_gather (t0 : Type) (t1 : Type) : forall (_ : Context_) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Binding) , (option) (Term)) , forall (_ : forall (_ : t0) , hydra.graph.Graph) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : t0) , t0) , forall (_ : bool) , forall (_ : t0) , forall (_ : (list) (Name)) , forall (_ : (list) (Name)) , forall (_ : (list) (Binding)) , forall (_ : (list) (Type_)) , forall (_ : (list) (Type_)) , forall (_ : Term) , (sum) (t1) ((FunctionStructure) (t0)) :=
  analyzeFunctionTermWith_gather_bundle.
Arguments analyzeFunctionTermWith_gather {t0} {t1}.
Definition analyzeFunctionTermWith (t0 : Type) (t1 : Type) : forall (_ : Context_) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Binding) , (option) (Term)) , forall (_ : forall (_ : t0) , hydra.graph.Graph) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : t0) , t0) , forall (_ : t0) , forall (_ : Term) , (sum) (t1) ((FunctionStructure) (t0)) := fun (cx : Context_) => fun (forBinding : forall (_ : hydra.graph.Graph) , forall (_ : Binding) , (option) (Term)) => fun (getTC : forall (_ : t0) , hydra.graph.Graph) => fun (setTC : forall (_ : hydra.graph.Graph) , forall (_ : t0) , t0) => fun (env : t0) => fun (term_ : Term) => ((((((((((((analyzeFunctionTermWith_gather) (cx)) (forBinding)) (getTC)) (setTC)) (true)) (env)) (nil)) (nil)) (nil)) (nil)) (nil)) (term_).
Arguments analyzeFunctionTermWith {t0} {t1}.
Definition analyzeFunctionTerm (t0 : Type) (t1 : Type) : forall (_ : Context_) , forall (_ : forall (_ : t0) , hydra.graph.Graph) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : t0) , t0) , forall (_ : t0) , forall (_ : Term) , (sum) (t1) ((FunctionStructure) (t0)) := fun (cx : Context_) => fun (getTC : forall (_ : t0) , hydra.graph.Graph) => fun (setTC : forall (_ : hydra.graph.Graph) , forall (_ : t0) , t0) => fun (env : t0) => fun (term_ : Term) => ((((((analyzeFunctionTermWith) (cx)) (fun (g : hydra.graph.Graph) => fun (b : Binding) => (((logic.ifElse) (((isComplexBinding) (g)) (b))) ((Some) ((Term_Literal) ((Literal_Boolean) (true))))) (None))) (getTC)) (setTC)) (env)) (term_).
Arguments analyzeFunctionTerm {t0} {t1}.
Definition definitionDependencyNamespaces : forall (_ : (list) (Definition_)) , (list) (Namespace) := fun (defs : (list) (Definition_)) => let defNames := fun (def : Definition_) => (fun x_ => match x_ with
| Definition__Type v_ => (fun (typeDef : TypeDefinition) => ((typeDependencyNames) (true)) ((fun r_ => (typeScheme_type) (r_)) ((fun r_ => (typeDefinition_type) (r_)) (typeDef)))) (v_)
| Definition__Term v_ => (fun (termDef : TermDefinition) => ((((termDependencyNames) (true)) (true)) (true)) ((fun r_ => (termDefinition_term) (r_)) (termDef))) (v_)
end) (def) in let allNames := (sets.unions) (((lists.map) (defNames)) (defs)) in (sets.fromList) ((maybes.cat) (((lists.map) (namespaceOf)) ((sets.toList) (allNames)))).
Definition dependencyNamespaces (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : bool) , forall (_ : bool) , forall (_ : bool) , forall (_ : bool) , forall (_ : (list) (Binding)) , (sum) (Error) ((list) (Namespace)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (binds : bool) => fun (withPrims : bool) => fun (withNoms : bool) => fun (withSchema : bool) => fun (els : (list) (Binding)) => let depNames := fun (el : Binding) => let term_ := (fun r_ => (binding_term) (r_)) (el) in let dataNames := ((((termDependencyNames) (binds)) (withPrims)) (withNoms)) (term_) in let deannotatedTerm := (deannotateTerm) (term_) in let schemaNames := (((logic.ifElse) (withSchema)) ((((maybes.maybe) (sets.empty)) (fun (ts : TypeScheme) => ((typeDependencyNames) (true)) ((fun r_ => (typeScheme_type) (r_)) (ts)))) ((fun r_ => (binding_type) (r_)) (el)))) (sets.empty) in (((logic.ifElse) ((isEncodedType) (deannotatedTerm))) (((eithers.map) (fun (typ : Type_) => (sets.unions) ((cons) (dataNames) ((cons) (schemaNames) ((cons) (((typeDependencyNames) (true)) (typ)) (nil)))))) ((((eithers.bimap) (fun (_e : DecodingError) => (Error_Decoding) (_e))) (fun (_a : Type_) => _a)) (((hydra.decode.core.type) (graph_)) (term_))))) ((((logic.ifElse) ((isEncodedTerm) (deannotatedTerm))) (((eithers.map) (fun (decodedTerm : Term) => (sets.unions) ((cons) (dataNames) ((cons) (schemaNames) ((cons) (((((termDependencyNames) (binds)) (withPrims)) (withNoms)) (decodedTerm)) (nil)))))) ((((eithers.bimap) (fun (_e : DecodingError) => (Error_Decoding) (_e))) (fun (_a : Term) => _a)) (((hydra.decode.core.term) (graph_)) (term_))))) ((inr) ((sets.unions) ((cons) (dataNames) ((cons) (schemaNames) (nil)))))) in ((eithers.map) (fun (namesList : (list) ((list) (Name))) => (sets.fromList) ((maybes.cat) (((lists.map) (namespaceOf)) ((sets.toList) ((sets.unions) (namesList))))))) (((eithers.mapList) (depNames)) (els)).
Arguments dependencyNamespaces {t0}.
Definition gatherApplications : forall (_ : Term) , (prod) ((list) (Term)) (Term) := fun (term_ : Term) => let go := (hydra_fix) (fun go => fun (args : (list) (Term)) => fun (t : Term) => (fun x_ => match x_ with
| Term_Application v_ => (fun (app : Application) => let lhs := (fun r_ => (application_function) (r_)) (app) in let rhs := (fun r_ => (application_argument) (r_)) (app) in ((go) (((lists.cons) (rhs)) (args))) (lhs)) (v_)
| _ => (pair) (args) (t)
end) ((deannotateTerm) (t))) in ((go) (nil)) (term_).
Definition gatherArgs_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , forall (_ : (list) (Term)) , (prod) (Term) ((list) (Term))) =>
    let gatherArgs := bundle_ in
    fun (term_ : Term) => fun (args : (list) (Term)) => (fun x_ => match x_ with
| Term_Application v_ => (fun (app : Application) => let lhs := (fun r_ => (application_function) (r_)) (app) in let rhs := (fun r_ => (application_argument) (r_)) (app) in ((gatherArgs) (lhs)) (((lists.cons) (rhs)) (args))) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => let body := (fun r_ => (typeLambda_body) (r_)) (tl) in ((gatherArgs) (body)) (args)) (v_)
| Term_TypeApplication v_ => (fun (ta : TypeApplicationTerm) => let body := (fun r_ => (typeApplicationTerm_body) (r_)) (ta) in ((gatherArgs) (body)) (args)) (v_)
| _ => (pair) (term_) (args)
end) ((deannotateTerm) (term_))).

Definition gatherArgs : forall (_ : Term) , forall (_ : (list) (Term)) , (prod) (Term) ((list) (Term)) :=
  gatherArgs_bundle.
Definition gatherArgsWithTypeApps_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , forall (_ : (list) (Term)) , forall (_ : (list) (Type_)) , (prod) (Term) ((prod) ((list) (Term)) ((list) (Type_)))) =>
    let gatherArgsWithTypeApps := bundle_ in
    fun (term_ : Term) => fun (args : (list) (Term)) => fun (tyArgs : (list) (Type_)) => (fun x_ => match x_ with
| Term_Application v_ => (fun (app : Application) => let lhs := (fun r_ => (application_function) (r_)) (app) in let rhs := (fun r_ => (application_argument) (r_)) (app) in (((gatherArgsWithTypeApps) (lhs)) (((lists.cons) (rhs)) (args))) (tyArgs)) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => let body := (fun r_ => (typeLambda_body) (r_)) (tl) in (((gatherArgsWithTypeApps) (body)) (args)) (tyArgs)) (v_)
| Term_TypeApplication v_ => (fun (ta : TypeApplicationTerm) => let body := (fun r_ => (typeApplicationTerm_body) (r_)) (ta) in let typ := (fun r_ => (typeApplicationTerm_type) (r_)) (ta) in (((gatherArgsWithTypeApps) (body)) (args)) (((lists.cons) (typ)) (tyArgs))) (v_)
| _ => (pair) (term_) ((pair) (args) (tyArgs))
end) ((deannotateTerm) (term_))).

Definition gatherArgsWithTypeApps : forall (_ : Term) , forall (_ : (list) (Term)) , forall (_ : (list) (Type_)) , (prod) (Term) ((prod) ((list) (Term)) ((list) (Type_))) :=
  gatherArgsWithTypeApps_bundle.
Definition isTailRecursiveInTailPosition_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Name) , forall (_ : Term) , bool) =>
    let isTailRecursiveInTailPosition := bundle_ in
    fun (funcName : Name) => fun (term_ : Term) => let stripped := (deannotateAndDetypeTerm) (term_) in (fun x_ => match x_ with
| Term_Application v_ => (fun (app : Application) => let gathered := (gatherApplications) (stripped) in let gatherArgs := (pairs.first) (gathered) in let gatherFun := (pairs.second) (gathered) in let strippedFun := (deannotateAndDetypeTerm) (gatherFun) in (fun x_ => match x_ with
| Term_Variable v_ => (fun (vname : Name) => (((logic.ifElse) (((equality.equal) (vname)) (funcName))) (let argsNoFunc := (((lists.foldl) (fun (ok : bool) => fun (arg : Term) => ((logic.and) (ok)) (((isFreeVariableInTerm) (funcName)) (arg)))) (true)) (gatherArgs) in let argsNoLambda := (((lists.foldl) (fun (ok : bool) => fun (arg : Term) => ((logic.and) (ok)) ((logic.not) (((((foldOverTerm) ((TraversalOrder_Pre) (tt))) (fun (found : bool) => fun (t : Term) => ((logic.or) (found)) ((fun x_ => match x_ with
| Term_Lambda v_ => (fun (lam : Lambda) => let ignore := (fun r_ => (lambda_body) (r_)) (lam) in true) (v_)
| _ => false
end) (t)))) (false)) (arg))))) (true)) (gatherArgs) in ((logic.and) (argsNoFunc)) (argsNoLambda))) (((isFreeVariableInTerm) (funcName)) (term_))) (v_)
| Term_Cases v_ => (fun (cs : CaseStatement) => let argsOk := (((lists.foldl) (fun (ok : bool) => fun (arg : Term) => ((logic.and) (ok)) (((isFreeVariableInTerm) (funcName)) (arg)))) (true)) (gatherArgs) in let cases_ := (fun r_ => (caseStatement_cases) (r_)) (cs) in let branchesOk := (((lists.foldl) (fun (ok : bool) => fun (field : Field) => ((logic.and) (ok)) (((isTailRecursiveInTailPosition) (funcName)) ((fun r_ => (field_term) (r_)) (field))))) (true)) (cases_) in let dflt := (fun r_ => (caseStatement_default) (r_)) (cs) in let dfltOk := (((maybes.maybe) (true)) (fun (d : Term) => ((isTailRecursiveInTailPosition) (funcName)) (d))) (dflt) in ((logic.and) (((logic.and) (branchesOk)) (dfltOk))) (argsOk)) (v_)
| _ => ((isFreeVariableInTerm) (funcName)) (term_)
end) (strippedFun)) (v_)
| Term_Lambda v_ => (fun (lam : Lambda) => ((isTailRecursiveInTailPosition) (funcName)) ((fun r_ => (lambda_body) (r_)) (lam))) (v_)
| Term_Let v_ => (fun (lt : Let) => let bindingsOk := (((lists.foldl) (fun (ok : bool) => fun (b : Binding) => ((logic.and) (ok)) (((isFreeVariableInTerm) (funcName)) ((fun r_ => (binding_term) (r_)) (b))))) (true)) ((fun r_ => (let_bindings) (r_)) (lt)) in ((logic.and) (bindingsOk)) (((isTailRecursiveInTailPosition) (funcName)) ((fun r_ => (let_body) (r_)) (lt)))) (v_)
| _ => ((isFreeVariableInTerm) (funcName)) (term_)
end) (stripped)).

Definition isTailRecursiveInTailPosition : forall (_ : Name) , forall (_ : Term) , bool :=
  isTailRecursiveInTailPosition_bundle.
Definition isSelfTailRecursive : forall (_ : Name) , forall (_ : Term) , bool := fun (funcName : Name) => fun (body : Term) => let callsSelf := (logic.not) (((isFreeVariableInTerm) (funcName)) (body)) in (((logic.ifElse) (callsSelf)) (((isTailRecursiveInTailPosition) (funcName)) (body))) (false).
Definition isSimpleAssignment_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , bool) =>
    let isSimpleAssignment := bundle_ in
    fun (term_ : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (isSimpleAssignment) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| Term_Lambda v_ => (fun (_ : Lambda) => false) (v_)
| Term_Let v_ => (fun (_ : Let) => false) (v_)
| Term_TypeLambda v_ => (fun (_ : TypeLambda) => false) (v_)
| Term_TypeApplication v_ => (fun (ta : TypeApplicationTerm) => (isSimpleAssignment) ((fun r_ => (typeApplicationTerm_body) (r_)) (ta))) (v_)
| _ => let baseTerm := (pairs.first) (((gatherArgs) (term_)) (nil)) in (fun x_ => match x_ with
| Term_Cases v_ => (fun (_ : CaseStatement) => false) (v_)
| _ => true
end) (baseTerm)
end) (term_)).

Definition isSimpleAssignment : forall (_ : Term) , bool :=
  isSimpleAssignment_bundle.
Definition moduleContainsBinaryLiterals : forall (_ : Module_) , bool := fun (mod_ : Module_) => let checkTerm := fun (found : bool) => fun (term_ : Term) => ((logic.or) (found)) ((fun x_ => match x_ with
| Term_Literal v_ => (fun (lit : Literal) => (fun x_ => match x_ with
| Literal_Binary v_ => (fun (_ : string) => true) (v_)
| _ => false
end) (lit)) (v_)
| _ => false
end) (term_)) in let defTerms := (maybes.cat) (((lists.map) (fun (d : Definition_) => (fun x_ => match x_ with
| Definition__Term v_ => (fun (td : TermDefinition) => (Some) ((fun r_ => (termDefinition_term) (r_)) (td))) (v_)
| _ => None
end) (d))) ((fun r_ => (module__definitions) (r_)) (mod_))) in let termContainsBinary := fun (term_ : Term) => ((((foldOverTerm) ((TraversalOrder_Pre) (tt))) (checkTerm)) (false)) (term_) in (((lists.foldl) (fun (acc : bool) => fun (t : Term) => ((logic.or) (acc)) ((termContainsBinary) (t)))) (false)) (defTerms).
Definition moduleDependencyNamespaces (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : bool) , forall (_ : bool) , forall (_ : bool) , forall (_ : bool) , forall (_ : Module_) , (sum) (Error) ((list) (Namespace)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (binds : bool) => fun (withPrims : bool) => fun (withNoms : bool) => fun (withSchema : bool) => fun (mod_ : Module_) => let allBindings := (maybes.cat) (((lists.map) (fun (d : Definition_) => (fun x_ => match x_ with
| Definition__Type v_ => (fun (td : TypeDefinition) => (Some) (((fun (name : Name) => fun (typ : Type_) => let schemaTerm := (Term_Variable) ("hydra.core.Type"%string) in let dataTerm := (normalizeTermAnnotations) ((Term_Annotated) ((Build_AnnotatedTerm) ((hydra.encode.core.type) (typ)) ((maps.fromList) ((cons) ((pair) (key_type) (schemaTerm)) (nil))))) in (Build_Binding) (name) (dataTerm) ((Some) ((Build_TypeScheme) (nil) ((Type__Variable) ("hydra.core.Type"%string)) (None)))) ((fun r_ => (typeDefinition_name) (r_)) (td))) ((fun r_ => (typeScheme_type) (r_)) ((fun r_ => (typeDefinition_type) (r_)) (td))))) (v_)
| Definition__Term v_ => (fun (td : TermDefinition) => (Some) ((Build_Binding) ((fun r_ => (termDefinition_name) (r_)) (td)) ((fun r_ => (termDefinition_term) (r_)) (td)) ((fun r_ => (termDefinition_type) (r_)) (td)))) (v_)
end) (d))) ((fun r_ => (module__definitions) (r_)) (mod_))) in ((eithers.map) (fun (deps : (list) (Namespace)) => ((sets.delete) ((fun r_ => (module__namespace) (r_)) (mod_))) (deps))) ((((((((dependencyNamespaces) (cx)) (graph_)) (binds)) (withPrims)) (withNoms)) (withSchema)) (allBindings)).
Arguments moduleDependencyNamespaces {t0}.
Definition namespacesForDefinitions (t0 : Type) : forall (_ : forall (_ : Namespace) , t0) , forall (_ : Namespace) , forall (_ : (list) (Definition_)) , (Namespaces) (t0) := fun (encodeNamespace : forall (_ : Namespace) , t0) => fun (focusNs : Namespace) => fun (defs : (list) (Definition_)) => let nss := ((sets.delete) (focusNs)) ((definitionDependencyNamespaces) (defs)) in let toPair := fun (ns : Namespace) => (pair) (ns) ((encodeNamespace) (ns)) in (Build_Namespaces) ((toPair) (focusNs)) ((maps.fromList) (((lists.map) (toPair)) ((sets.toList) (nss)))).
Arguments namespacesForDefinitions {t0}.

