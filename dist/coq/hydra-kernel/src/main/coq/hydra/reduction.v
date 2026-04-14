(* Functions for reducing terms and types, i.e. performing computations. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.lists hydra.lib.logic hydra.lib.maybes hydra.lib.eithers hydra.lib.pairs hydra.lib.maps hydra.lib.sets hydra.strip hydra.variables hydra.context hydra.graph hydra.errors hydra.show.errors hydra.extract.core hydra.lib.equality hydra.lexical hydra.arity hydra.rewriting hydra.lib.math hydra.lib.strings hydra.lib.literals hydra.checking hydra.scoping hydra.encode.core hydra.resolution.

Definition termIsValue_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , bool) =>
    let termIsValue := bundle_ in
    fun (term_ : Term) => let forList := fun (els : (list) (Term)) => (((lists.foldl) (fun (b : bool) => fun (t : Term) => ((logic.and) (b)) ((termIsValue) (t)))) (true)) (els) in let checkField := fun (f : Field) => (termIsValue) ((fun r_ => (field_term) (r_)) (f)) in let checkFields := fun (fields : (list) (Field)) => (((lists.foldl) (fun (b : bool) => fun (f : Field) => ((logic.and) (b)) ((checkField) (f)))) (true)) (fields) in (fun x_ => match x_ with
| Term_Application v_ => (fun (_ : Application) => false) (v_)
| Term_Cases v_ => (fun (cs : CaseStatement) => ((logic.and) ((checkFields) ((fun r_ => (caseStatement_cases) (r_)) (cs)))) ((((maybes.maybe) (true)) (termIsValue)) ((fun r_ => (caseStatement_default) (r_)) (cs)))) (v_)
| Term_Either v_ => (fun (e : (sum) (Term) (Term)) => (((eithers.either) (fun (l : Term) => (termIsValue) (l))) (fun (r : Term) => (termIsValue) (r))) (e)) (v_)
| Term_Lambda v_ => (fun (l : Lambda) => (termIsValue) ((fun r_ => (lambda_body) (r_)) (l))) (v_)
| Term_Literal v_ => (fun (_ : Literal) => true) (v_)
| Term_Project v_ => (fun (_ : Projection) => true) (v_)
| Term_Unwrap v_ => (fun (_ : Name) => true) (v_)
| Term_List v_ => (fun (els : (list) (Term)) => (forList) (els)) (v_)
| Term_Map v_ => (fun (m : (list) ((prod) (Term) (Term))) => (((lists.foldl) (fun (b : bool) => fun (kv : (prod) (Term) (Term)) => ((logic.and) (b)) (((logic.and) ((termIsValue) ((pairs.first) (kv)))) ((termIsValue) ((pairs.second) (kv)))))) (true)) ((maps.toList) (m))) (v_)
| Term_Maybe v_ => (fun (m : (option) (Term)) => (((maybes.maybe) (true)) (termIsValue)) (m)) (v_)
| Term_Record v_ => (fun (r : Record_) => (checkFields) ((fun r_ => (record__fields) (r_)) (r))) (v_)
| Term_Set v_ => (fun (s : (list) (Term)) => (forList) ((sets.toList) (s))) (v_)
| Term_Inject v_ => (fun (i : Injection) => (checkField) ((fun r_ => (injection_field) (r_)) (i))) (v_)
| Term_Unit _ => true
| Term_Variable v_ => (fun (_ : Name) => false) (v_)
| _ => false
end) ((deannotateTerm) (term_))).

Definition termIsValue : forall (_ : Term) , bool :=
  termIsValue_bundle.
Definition termIsClosed : forall (_ : Term) , bool := fun (term_ : Term) => (sets.null) ((freeVariablesInTerm) (term_)).
Definition reduceTerm_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : bool) , forall (_ : Term) , (sum) (Error) (Term)) =>
    let reduceTerm := bundle_ in
    fun (cx : Context_) => fun (graph_ : hydra.graph.Graph) => fun (eager : bool) => fun (term_ : Term) => let reduce := fun (eager2 : bool) => (((reduceTerm) (cx)) (graph_)) (eager2) in let reduceArg := fun (eager2 : bool) => fun (arg : Term) => (((logic.ifElse) (eager2)) ((inr) (arg))) (((reduce) (false)) (arg)) in let mapErrorToString := fun (e : Error) => (Error_Other) ((hydra.show.errors.error) (e)) in let doRecurse := fun (eager2 : bool) => fun (term2 : Term) => let isNonLambdaTerm := (fun x_ => match x_ with
| Term_Lambda v_ => (fun (_ : Lambda) => false) (v_)
| Term_Let v_ => (fun (_ : Let) => false) (v_)
| _ => true
end) (term2) in ((logic.and) (eager2)) (isNonLambdaTerm) in let applyToArguments := (hydra_fix) (fun applyToArguments => fun (fun_ : Term) => fun (args : (list) (Term)) => (((logic.ifElse) ((lists.null) (args))) (fun_)) (((applyToArguments) ((Term_Application) ((Build_Application) (fun_) ((lists.head) (args))))) ((lists.tail) (args)))) in let applyProjection := fun (proj : Projection) => fun (reducedArg : Term) => ((eithers.bind) ((((hydra.extract.core.record) ((fun r_ => (projection_typeName) (r_)) (proj))) (graph_)) ((deannotateTerm) (reducedArg)))) (fun (fields : (list) (Field)) => let matchingFields := ((lists.filter) (fun (f : Field) => ((equality.equal) ((fun r_ => (field_name) (r_)) (f))) ((fun r_ => (projection_field) (r_)) (proj)))) (fields) in (((logic.ifElse) ((lists.null) (matchingFields))) ((inl) ((Error_Resolution) ((ResolutionError_NoMatchingField) ((Build_NoMatchingFieldError) ((fun r_ => (projection_field) (r_)) (proj))))))) ((inr) ((fun r_ => (field_term) (r_)) ((lists.head) (matchingFields))))) in let applyCases := fun (cs : CaseStatement) => fun (reducedArg : Term) => ((eithers.bind) ((((hydra.extract.core.injection) ((fun r_ => (caseStatement_typeName) (r_)) (cs))) (graph_)) (reducedArg))) (fun (field : Field) => let matchingFields := ((lists.filter) (fun (f : Field) => ((equality.equal) ((fun r_ => (field_name) (r_)) (f))) ((fun r_ => (field_name) (r_)) (field)))) ((fun r_ => (caseStatement_cases) (r_)) (cs)) in (((logic.ifElse) ((lists.null) (matchingFields))) ((((maybes.maybe) ((inl) ((Error_Resolution) ((ResolutionError_NoMatchingField) ((Build_NoMatchingFieldError) ((fun r_ => (field_name) (r_)) (field))))))) (fun (x : Term) => (inr) (x))) ((fun r_ => (caseStatement_default) (r_)) (cs)))) ((inr) ((Term_Application) ((Build_Application) ((fun r_ => (field_term) (r_)) ((lists.head) (matchingFields))) ((fun r_ => (field_term) (r_)) (field)))))) in let applyIfNullary := (hydra_fix) (fun applyIfNullary => fun (eager2 : bool) => fun (original : Term) => fun (args : (list) (Term)) => let stripped := (deannotateTerm) (original) in let forUnwrap := fun (name : Name) => fun (args2 : (list) (Term)) => let remainingArgs := (lists.tail) (args2) in let arg := (lists.head) (args2) in ((eithers.bind) (((reduceArg) (eager2)) ((deannotateTerm) (arg)))) (fun (reducedArg : Term) => ((eithers.bind) (((eithers.bind) ((((wrap) (name)) (graph_)) (reducedArg))) ((reduce) (eager2)))) (fun (reducedResult : Term) => (((applyIfNullary) (eager2)) (reducedResult)) (remainingArgs))) in let forProjection := fun (proj : Projection) => fun (args2 : (list) (Term)) => let remainingArgs := (lists.tail) (args2) in let arg := (lists.head) (args2) in ((eithers.bind) (((reduceArg) (eager2)) ((deannotateTerm) (arg)))) (fun (reducedArg : Term) => ((eithers.bind) (((eithers.bind) (((applyProjection) (proj)) (reducedArg))) ((reduce) (eager2)))) (fun (reducedResult : Term) => (((applyIfNullary) (eager2)) (reducedResult)) (remainingArgs))) in let forPrimitive := fun (prim : Primitive) => fun (arity : Z) => fun (args2 : (list) (Term)) => let remainingArgs := ((lists.drop) (arity)) (args2) in let argList := ((lists.take) (arity)) (args2) in ((eithers.bind) (((eithers.mapList) ((reduceArg) (eager2))) (argList))) (fun (reducedArgs : (list) (Term)) => let strippedArgs := ((lists.map) (deannotateTerm)) (reducedArgs) in ((eithers.bind) ((((eithers.bimap) (mapErrorToString)) (fun (x : Term) => x)) (((((fun _ : _ => hydra_unreachable) (prim)) (cx)) (graph_)) (strippedArgs)))) (fun (primResult : Term) => ((eithers.bind) (((reduce) (eager2)) (primResult))) (fun (reducedResult : Term) => (((applyIfNullary) (eager2)) (reducedResult)) (remainingArgs)))) in let forLambda := fun (l : Lambda) => fun (args2 : (list) (Term)) => let remainingArgs := (lists.tail) (args2) in let param := (fun r_ => (lambda_parameter) (r_)) (l) in let body := (fun r_ => (lambda_body) (r_)) (l) in let arg := (lists.head) (args2) in ((eithers.bind) (((reduce) (eager2)) ((deannotateTerm) (arg)))) (fun (reducedArg : Term) => ((eithers.bind) (((reduce) (eager2)) ((((replaceFreeTermVariable) (param)) (reducedArg)) (body)))) (fun (reducedResult : Term) => (((applyIfNullary) (eager2)) (reducedResult)) (remainingArgs))) in let forCases := fun (cs : CaseStatement) => fun (args2 : (list) (Term)) => let remainingArgs := (lists.tail) (args2) in let arg := (lists.head) (args2) in ((eithers.bind) (((reduceArg) (eager2)) ((deannotateTerm) (arg)))) (fun (reducedArg : Term) => ((eithers.bind) (((eithers.bind) (((applyCases) (cs)) (reducedArg))) ((reduce) (eager2)))) (fun (reducedResult : Term) => (((applyIfNullary) (eager2)) (reducedResult)) (remainingArgs))) in (fun x_ => match x_ with
| Term_Application v_ => (fun (app : Application) => (((applyIfNullary) (eager2)) ((fun r_ => (application_function) (r_)) (app))) (((lists.cons) ((fun r_ => (application_argument) (r_)) (app))) (args))) (v_)
| Term_Cases v_ => (fun (cs : CaseStatement) => (((logic.ifElse) ((lists.null) (args))) ((inr) (original))) (((forCases) (cs)) (args))) (v_)
| Term_Project v_ => (fun (p : Projection) => (((logic.ifElse) ((lists.null) (args))) ((inr) (original))) (((forProjection) (p)) (args))) (v_)
| Term_Unwrap v_ => (fun (n : Name) => (((logic.ifElse) ((lists.null) (args))) ((inr) (original))) (((forUnwrap) (n)) (args))) (v_)
| Term_Lambda v_ => (fun (l : Lambda) => (((logic.ifElse) ((lists.null) (args))) ((inr) (original))) (((forLambda) (l)) (args))) (v_)
| Term_Variable v_ => (fun (v : Name) => let mBinding := ((lookupBinding) (graph_)) (v) in (((maybes.maybe) (let mPrim := ((lookupPrimitive) (graph_)) (v) in (((maybes.maybe) ((inr) (((applyToArguments) (original)) (args)))) (fun (prim : Primitive) => let arity := (primitiveArity) (prim) in (((logic.ifElse) (((equality.gt) (arity)) ((lists.length) (args)))) ((inr) (((applyToArguments) (original)) (args)))) ((((forPrimitive) (prim)) (arity)) (args)))) (mPrim))) (fun (binding : Binding) => (((applyIfNullary) (eager2)) ((fun r_ => (binding_term) (r_)) (binding))) (args))) (mBinding)) (v_)
| Term_Let v_ => (fun (lt : Let) => let substituteBinding := fun (term2 : Term) => fun (b : Binding) => (((replaceFreeTermVariable) ((fun r_ => (binding_name) (r_)) (b))) ((fun r_ => (binding_term) (r_)) (b))) (term2) in let substituteAll := fun (bs : (list) (Binding)) => fun (term2 : Term) => (((lists.foldl) (substituteBinding)) (term2)) (bs) in let letExpr := fun (b : Binding) => (Term_Let) ((Build_Let) ((cons) (b) (nil)) ((Term_Variable) ((fun r_ => (binding_name) (r_)) (b)))) in let expandBinding := fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) ((((replaceFreeTermVariable) ((fun r_ => (binding_name) (r_)) (b))) ((letExpr) (b))) ((fun r_ => (binding_term) (r_)) (b))) ((fun r_ => (binding_type) (r_)) (b)) in let body := (fun r_ => (let_body) (r_)) (lt) in let bindings := (fun r_ => (let_bindings) (r_)) (lt) in let expandedBindings := ((lists.map) (expandBinding)) (bindings) in let expandedBody := ((substituteAll) (expandedBindings)) (body) in ((eithers.bind) (((reduce) (eager2)) (expandedBody))) (fun (reducedBody : Term) => (((applyIfNullary) (eager2)) (reducedBody)) (args))) (v_)
| _ => (inr) (((applyToArguments) (original)) (args))
end) (stripped)) in let mapping := fun (recurse : forall (_ : Term) , (sum) (Error) (Term)) => fun (mid : Term) => ((eithers.bind) ((((logic.ifElse) (((doRecurse) (eager)) (mid))) ((recurse) (mid))) ((inr) (mid)))) (fun (inner : Term) => (((applyIfNullary) (eager)) (inner)) (nil)) in ((rewriteTermM) (mapping)) (term_)).

Definition reduceTerm : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : bool) , forall (_ : Term) , (sum) (Error) (Term) :=
  reduceTerm_bundle.
Definition etaReduceTerm_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , Term) =>
    let etaReduceTerm := bundle_ in
    fun (term_ : Term) => let noChange := term_ in let reduceLambda := (hydra_fix) (fun reduceLambda => fun (l : Lambda) => let v := (fun r_ => (lambda_parameter) (r_)) (l) in let d := (fun r_ => (lambda_domain) (r_)) (l) in let body := (fun r_ => (lambda_body) (r_)) (l) in (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (reduceLambda) ((Build_Lambda) (v) (d) ((fun r_ => (annotatedTerm_body) (r_)) (at_)))) (v_)
| Term_Application v_ => (fun (app : Application) => let rhs := (fun r_ => (application_argument) (r_)) (app) in let lhs := (fun r_ => (application_function) (r_)) (app) in (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (reduceLambda) ((Build_Lambda) (v) (d) ((Term_Application) ((Build_Application) (lhs) ((fun r_ => (annotatedTerm_body) (r_)) (at_)))))) (v_)
| Term_Variable v_ => (fun (v1 : Name) => (((logic.ifElse) (((logic.and) (((equality.equal) ((fun w_ => w_) (v))) ((fun w_ => w_) (v1)))) ((logic.not) (((isFreeVariableInTerm) (v)) (lhs))))) ((etaReduceTerm) (lhs))) (noChange)) (v_)
| _ => noChange
end) ((etaReduceTerm) (rhs))) (v_)
| _ => noChange
end) ((etaReduceTerm) (body))) in (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (Term_Annotated) ((Build_AnnotatedTerm) ((etaReduceTerm) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) ((fun r_ => (annotatedTerm_annotation) (r_)) (at_)))) (v_)
| Term_Lambda v_ => (fun (l : Lambda) => (reduceLambda) (l)) (v_)
| _ => noChange
end) (term_)).

Definition etaReduceTerm : forall (_ : Term) , Term :=
  etaReduceTerm_bundle.
Definition etaExpansionArity_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , Z) =>
    let etaExpansionArity := bundle_ in
    fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => ((etaExpansionArity) (graph_)) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| Term_Application v_ => (fun (app : Application) => ((math.sub) (((etaExpansionArity) (graph_)) ((fun r_ => (application_function) (r_)) (app)))) ((1)%Z)) (v_)
| Term_Cases v_ => (fun (_ : CaseStatement) => (1)%Z) (v_)
| Term_Lambda v_ => (fun (_ : Lambda) => (0)%Z) (v_)
| Term_Project v_ => (fun (_ : Projection) => (1)%Z) (v_)
| Term_Unwrap v_ => (fun (_ : Name) => (1)%Z) (v_)
| Term_TypeLambda v_ => (fun (ta : TypeLambda) => ((etaExpansionArity) (graph_)) ((fun r_ => (typeLambda_body) (r_)) (ta))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => ((etaExpansionArity) (graph_)) ((fun r_ => (typeApplicationTerm_body) (r_)) (tt))) (v_)
| Term_Variable v_ => (fun (name : Name) => (((maybes.maybe) ((0)%Z)) (fun (ts : TypeScheme) => (typeArity) ((fun r_ => (typeScheme_type) (r_)) (ts)))) (((maybes.bind) (((lookupBinding) (graph_)) (name))) (fun (b : Binding) => (fun r_ => (binding_type) (r_)) (b)))) (v_)
| _ => (0)%Z
end) (term_)).

Definition etaExpansionArity : forall (_ : hydra.graph.Graph) , forall (_ : Term) , Z :=
  etaExpansionArity_bundle.
Definition etaExpandTypedTerm : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term) := fun (cx : Context_) => fun (tx0 : hydra.graph.Graph) => fun (term0 : Term) => let rewrite := (hydra_fix) (fun rewrite => fun (topLevel : bool) => fun (forced : bool) => fun (typeArgs : (list) (Type_)) => fun (recurse : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term)) => fun (tx : hydra.graph.Graph) => fun (term_ : Term) => let unwind := fun (term2 : Term) => (((lists.foldl) (fun (e : Term) => fun (t : Type_) => (Term_TypeApplication) ((Build_TypeApplicationTerm) (e) (t)))) (term2)) (typeArgs) in let rewriteSpine := (hydra_fix) (fun rewriteSpine => fun (term2 : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => ((eithers.bind) ((rewriteSpine) ((fun r_ => (annotatedTerm_body) (r_)) (at_)))) (fun (body : Term) => let ann := (fun r_ => (annotatedTerm_annotation) (r_)) (at_) in (inr) ((Term_Annotated) ((Build_AnnotatedTerm) (body) (ann))))) (v_)
| Term_Application v_ => (fun (a : Application) => let l := (((logic.ifElse) (false)) ((cons) ((Type__Literal) ((LiteralType_String) (tt))) (nil))) (nil) in ((eithers.bind) ((rewriteSpine) ((fun r_ => (application_function) (r_)) (a)))) (fun (lhs : Term) => ((eithers.bind) (((((((rewrite) (true)) (false)) (l)) (recurse)) (tx)) ((fun r_ => (application_argument) (r_)) (a)))) (fun (rhs : Term) => (inr) ((Term_Application) ((Build_Application) (lhs) (rhs)))))) (v_)
| Term_TypeApplication v_ => (fun (tat : TypeApplicationTerm) => ((eithers.bind) ((rewriteSpine) ((fun r_ => (typeApplicationTerm_body) (r_)) (tat)))) (fun (body : Term) => let typ := (fun r_ => (typeApplicationTerm_type) (r_)) (tat) in (inr) ((Term_TypeApplication) ((Build_TypeApplicationTerm) (body) (typ))))) (v_)
| _ => ((((((rewrite) (false)) (false)) (nil)) (recurse)) (tx)) (term2)
end) (term2)) in let pad := (hydra_fix) (fun pad => fun (vars : (list) (Name)) => fun (body : Term) => (((logic.ifElse) ((lists.null) (vars))) (body)) ((Term_Lambda) ((Build_Lambda) ((lists.head) (vars)) (None) (((pad) ((lists.tail) (vars))) ((Term_Application) ((Build_Application) (body) ((Term_Variable) ((lists.head) (vars))))))))) in let forCase := fun (f : Field) => ((eithers.bind) (((((((rewrite) (false)) (true)) (nil)) (recurse)) (tx)) ((fun r_ => (field_term) (r_)) (f)))) (fun (r : Term) => (inr) ((Build_Field) ((fun r_ => (field_name) (r_)) (f)) (r))) in let forCaseStatement := fun (cs : CaseStatement) => let tname := (fun r_ => (caseStatement_typeName) (r_)) (cs) in let dflt := (fun r_ => (caseStatement_default) (r_)) (cs) in let csCases := (fun r_ => (caseStatement_cases) (r_)) (cs) in ((eithers.bind) (((eithers.mapMaybe) ((((((rewrite) (false)) (false)) (nil)) (recurse)) (tx))) (dflt))) (fun (rdflt : (option) (Term)) => ((eithers.bind) (((eithers.mapList) (forCase)) (csCases))) (fun (rcases : (list) (Field)) => (inr) ((Term_Cases) ((Build_CaseStatement) (tname) (rdflt) (rcases))))) in let extraVariables := fun (n : Z) => ((lists.map) (fun (i : Z) => ((strings.cat2) ("v"%string)) ((literals.showInt32) (i)))) (((math.range) ((1)%Z)) (n)) in let padn := fun (n : Z) => fun (body : Term) => ((pad) ((extraVariables) (n))) (body) in let forCases := fun (cs : CaseStatement) => ((eithers.bind) (((eithers.map) (unwind)) ((forCaseStatement) (cs)))) (fun (base : Term) => (inr) ((((logic.ifElse) (((logic.or) (topLevel)) (forced))) (((padn) ((1)%Z)) (base))) (base))) in let forNullaryElim := fun (elimTerm : Term) => let base := (unwind) (elimTerm) in (((logic.ifElse) (((logic.or) (topLevel)) (forced))) (((padn) ((1)%Z)) (base))) (base) in let forceExpansion := fun (t : Term) => ((eithers.bind) (((((typeOf) (cx)) (tx)) (nil)) (t))) (fun (typCx : (prod) (Type_) (Context_)) => let arity := (typeArity) ((pairs.first) (typCx)) in (inr) (((padn) (arity)) ((unwind) (t)))) in let recurseOrForce := fun (term2 : Term) => (((logic.ifElse) (forced)) ((forceExpansion) (term2))) (((recurse) (tx)) ((unwind) (term2))) in let arityOf := (hydra_fix) (fun arityOf => fun (tx2 : hydra.graph.Graph) => fun (term2 : Term) => let dflt := ((eithers.map) (fun (_tc : (prod) (Type_) (Context_)) => (typeArity) ((pairs.first) (_tc)))) (((((typeOf) (cx)) (tx2)) (nil)) (term2)) in (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => ((arityOf) (tx2)) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| Term_Cases v_ => (fun (_ : CaseStatement) => (inr) ((1)%Z)) (v_)
| Term_Project v_ => (fun (_ : Projection) => (inr) ((1)%Z)) (v_)
| Term_Unwrap v_ => (fun (_ : Name) => (inr) ((1)%Z)) (v_)
| Term_Lambda v_ => (fun (l : Lambda) => let txl := ((extendGraphForLambda) (tx2)) (l) in ((arityOf) (txl)) ((fun r_ => (lambda_body) (r_)) (l))) (v_)
| Term_Let v_ => (fun (l : Let) => let txl := (((extendGraphForLet) (fun (_ : hydra.graph.Graph) => fun (_2 : Binding) => None)) (tx2)) (l) in ((arityOf) (txl)) ((fun r_ => (let_body) (r_)) (l))) (v_)
| Term_TypeApplication v_ => (fun (tat : TypeApplicationTerm) => ((arityOf) (tx2)) ((fun r_ => (typeApplicationTerm_body) (r_)) (tat))) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => let txt := ((extendGraphForTypeLambda) (tx2)) (tl) in ((arityOf) (txt)) ((fun r_ => (typeLambda_body) (r_)) (tl))) (v_)
| Term_Variable v_ => (fun (name : Name) => (((maybes.maybe) (((eithers.map) (fun (_tc : (prod) (Type_) (Context_)) => (typeArity) ((pairs.first) (_tc)))) (((((typeOf) (cx)) (tx2)) (nil)) ((Term_Variable) (name))))) (fun (t : Type_) => (inr) ((typeArity) (t)))) (((maybes.map) (typeSchemeToFType)) (((maps.lookup) (name)) ((fun r_ => (graph_boundTypes) (r_)) (tx2))))) (v_)
| _ => dflt
end) (term2)) in (fun x_ => match x_ with
| Term_Application v_ => (fun (a : Application) => let rhs := (fun r_ => (application_argument) (r_)) (a) in let lhs := (fun r_ => (application_function) (r_)) (a) in ((eithers.bind) (((((((rewrite) (true)) (false)) (nil)) (recurse)) (tx)) (rhs))) (fun (rhs2 : Term) => ((eithers.bind) (((arityOf) (tx)) (lhs))) (fun (lhsarity : Z) => ((eithers.bind) ((rewriteSpine) (lhs))) (fun (lhs2 : Term) => let a2 := (Term_Application) ((Build_Application) (lhs2) (rhs2)) in (inr) ((((logic.ifElse) (((equality.gt) (lhsarity)) ((1)%Z))) (((padn) (((math.sub) (lhsarity)) ((1)%Z))) (a2))) (a2)))))) (v_)
| Term_Cases v_ => (fun (cs : CaseStatement) => (forCases) (cs)) (v_)
| Term_Project v_ => (fun (p : Projection) => (inr) ((forNullaryElim) ((Term_Project) (p)))) (v_)
| Term_Unwrap v_ => (fun (n : Name) => (inr) ((forNullaryElim) ((Term_Unwrap) (n)))) (v_)
| Term_Lambda v_ => (fun (l : Lambda) => let txl := ((extendGraphForLambda) (tx)) (l) in ((eithers.map) (unwind)) (((recurse) (txl)) (term_))) (v_)
| Term_Let v_ => (fun (l : Let) => let txlt := (((extendGraphForLet) (fun (_ : hydra.graph.Graph) => fun (_2 : Binding) => None)) (tx)) (l) in ((recurse) (txlt)) (term_)) (v_)
| Term_TypeApplication v_ => (fun (tat : TypeApplicationTerm) => ((((((rewrite) (topLevel)) (forced)) (((lists.cons) ((fun r_ => (typeApplicationTerm_type) (r_)) (tat))) (typeArgs))) (recurse)) (tx)) ((fun r_ => (typeApplicationTerm_body) (r_)) (tat))) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => let txt := ((extendGraphForTypeLambda) (tx)) (tl) in ((recurse) (txt)) (term_)) (v_)
| _ => (recurseOrForce) (term_)
end) (term_)) in (((rewriteTermWithContextM) ((((rewrite) (true)) (false)) (nil))) (tx0)) (term0).
Definition countPrimitiveInvocations : bool := true.
Definition contractTerm : forall (_ : Term) , Term := fun (term_ : Term) => let rewrite := fun recurse => fun t => let rec := (recurse) (t) in (fun x_ => match x_ with
| Term_Application v_ => (fun (app : Application) => let rhs := (fun r_ => (application_argument) (r_)) (app) in let lhs := (fun r_ => (application_function) (r_)) (app) in (fun x_ => match x_ with
| Term_Lambda v_ => (fun (l : Lambda) => let v := (fun r_ => (lambda_parameter) (r_)) (l) in let body := (fun r_ => (lambda_body) (r_)) (l) in (((logic.ifElse) (((isFreeVariableInTerm) (v)) (body))) (body)) ((((replaceFreeTermVariable) (v)) (rhs)) (body))) (v_)
| _ => rec
end) ((deannotateTerm) (lhs))) (v_)
| _ => rec
end) (rec) in ((rewriteTerm) (rewrite)) (term_).
Definition etaExpandTerm : forall (_ : hydra.graph.Graph) , forall (_ : Term) , Term := fun (tx0 : hydra.graph.Graph) => fun (term0 : Term) => let primTypes := (maps.fromList) (((lists.map) (fun (_gpt_p : Primitive) => (pair) ((fun r_ => (primitive_name) (r_)) (_gpt_p)) ((fun r_ => (primitive_type) (r_)) (_gpt_p)))) ((maps.elems) ((fun r_ => (graph_primitives) (r_)) (tx0)))) in let termArityWithContext := (hydra_fix) (fun termArityWithContext => fun (tx : hydra.graph.Graph) => fun (term_ : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => ((termArityWithContext) (tx)) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| Term_Application v_ => (fun (app : Application) => ((math.sub) (((termArityWithContext) (tx)) ((fun r_ => (application_function) (r_)) (app)))) ((1)%Z)) (v_)
| Term_Cases v_ => (fun (_ : CaseStatement) => (1)%Z) (v_)
| Term_Lambda v_ => (fun (_ : Lambda) => (0)%Z) (v_)
| Term_Project v_ => (fun (_ : Projection) => (1)%Z) (v_)
| Term_Unwrap v_ => (fun (_ : Name) => (1)%Z) (v_)
| Term_Let v_ => (fun (l : Let) => ((termArityWithContext) ((((extendGraphForLet) (fun (_ : hydra.graph.Graph) => fun (_2 : Binding) => None)) (tx)) (l))) ((fun r_ => (let_body) (r_)) (l))) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => ((termArityWithContext) (((extendGraphForTypeLambda) (tx)) (tl))) ((fun r_ => (typeLambda_body) (r_)) (tl))) (v_)
| Term_TypeApplication v_ => (fun (tat : TypeApplicationTerm) => ((termArityWithContext) (tx)) ((fun r_ => (typeApplicationTerm_body) (r_)) (tat))) (v_)
| Term_Variable v_ => (fun (name : Name) => (((maybes.maybe) ((((maybes.maybe) ((0)%Z)) (typeSchemeArity)) (((maps.lookup) (name)) (primTypes)))) (typeArity)) (((maybes.map) (typeSchemeToFType)) (((maps.lookup) (name)) ((fun r_ => (graph_boundTypes) (r_)) (tx))))) (v_)
| _ => (0)%Z
end) (term_)) in let peelFunctionDomains := (hydra_fix) (fun peelFunctionDomains => fun (mtyp : (option) (Type_)) => fun (n : Z) => (((logic.ifElse) (((equality.lte) (n)) ((0)%Z))) (mtyp)) ((((maybes.maybe) (None)) (fun (typ : Type_) => (fun x_ => match x_ with
| Type__Function v_ => (fun (ftyp : FunctionType) => ((peelFunctionDomains) ((Some) ((fun r_ => (functionType_codomain) (r_)) (ftyp)))) (((math.sub) (n)) ((1)%Z))) (v_)
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => ((peelFunctionDomains) ((Some) ((fun r_ => (annotatedType_body) (r_)) (at_)))) (n)) (v_)
| Type__Application v_ => (fun (atyp : ApplicationType) => ((peelFunctionDomains) ((Some) ((fun r_ => (applicationType_function) (r_)) (atyp)))) (n)) (v_)
| Type__Forall v_ => (fun (_ : ForallType) => None) (v_)
| _ => None
end) (typ))) (mtyp))) in let domainTypes := (hydra_fix) (fun domainTypes => fun (n : Z) => fun (mt : (option) (Type_)) => (((logic.ifElse) (((equality.lte) (n)) ((0)%Z))) (nil)) ((((maybes.maybe) (((lists.map) (fun (_ : Z) => None)) (((math.range) ((1)%Z)) (n)))) (fun (typ : Type_) => (fun x_ => match x_ with
| Type__Function v_ => (fun (ftyp : FunctionType) => ((lists.cons) ((Some) ((fun r_ => (functionType_domain) (r_)) (ftyp)))) (((domainTypes) (((math.sub) (n)) ((1)%Z))) ((Some) ((fun r_ => (functionType_codomain) (r_)) (ftyp))))) (v_)
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => ((domainTypes) (n)) ((Some) ((fun r_ => (annotatedType_body) (r_)) (at_)))) (v_)
| Type__Application v_ => (fun (atyp : ApplicationType) => ((domainTypes) (n)) ((Some) ((fun r_ => (applicationType_function) (r_)) (atyp)))) (v_)
| Type__Forall v_ => (fun (_ : ForallType) => ((lists.map) (fun (_2 : Z) => None)) (((math.range) ((1)%Z)) (n))) (v_)
| _ => ((lists.map) (fun (_ : Z) => None)) (((math.range) ((1)%Z)) (n))
end) (typ))) (mt))) in let expand := fun (alwaysPad : bool) => fun (args : (list) (Term)) => fun (arity : Z) => fun (headTyp : (option) (Type_)) => fun (head : Term) => let numArgs := (lists.length) (args) in let needed := ((math.sub) (arity)) (numArgs) in let applied := (((lists.foldl) (fun (lhs : Term) => fun (arg : Term) => (Term_Application) ((Build_Application) (lhs) (arg)))) (head)) (args) in (((logic.ifElse) (((logic.and) (((equality.gt) (needed)) ((0)%Z))) (((logic.or) (alwaysPad)) (((equality.gt) (numArgs)) ((0)%Z))))) (let remainingType := ((peelFunctionDomains) (headTyp)) (numArgs) in let indices := ((math.range) ((1)%Z)) (needed) in let fullyAppliedRaw := (((lists.foldl) (fun (body : Term) => fun (i : Z) => let vn := ((strings.cat2) ("v"%string)) ((literals.showInt32) (i)) in (Term_Application) ((Build_Application) (body) ((Term_Variable) (vn))))) (applied)) (indices) in let domains := ((domainTypes) (needed)) (remainingType) in let indexedDomains := ((lists.zip) (indices)) (domains) in let codomainType := ((peelFunctionDomains) (remainingType)) (needed) in let fullyApplied := (((maybes.maybe) (fullyAppliedRaw)) (fun (ct : Type_) => (Term_Annotated) ((Build_AnnotatedTerm) (fullyAppliedRaw) (((maps.singleton) ("type"%string)) ((hydra.encode.core.type) (ct)))))) (codomainType) in (((lists.foldl) (fun (body : Term) => fun (idPair : (prod) (Z) ((option) (Type_))) => let i := (pairs.first) (idPair) in let vn := ((strings.cat2) ("v"%string)) ((literals.showInt32) (i)) in let dom := (pairs.second) (idPair) in (Term_Lambda) ((Build_Lambda) (vn) (dom) (body)))) (fullyApplied)) ((lists.reverse) (indexedDomains)))) (applied) in let rewriteWithArgs := (hydra_fix) (fun rewriteWithArgs => fun (args : (list) (Term)) => fun (tx : hydra.graph.Graph) => fun (term_ : Term) => let termHeadType := (hydra_fix) (fun termHeadType => fun (tx2 : hydra.graph.Graph) => fun (trm2 : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at2 : AnnotatedTerm) => ((termHeadType) (tx2)) ((fun r_ => (annotatedTerm_body) (r_)) (at2))) (v_)
| Term_Lambda v_ => (fun (_ : Lambda) => None) (v_)
| Term_Cases v_ => (fun (_ : CaseStatement) => None) (v_)
| Term_Project v_ => (fun (_ : Projection) => None) (v_)
| Term_Unwrap v_ => (fun (_ : Name) => None) (v_)
| Term_Let v_ => (fun (l2 : Let) => ((termHeadType) ((((extendGraphForLet) (fun (_ : hydra.graph.Graph) => fun (_2 : Binding) => None)) (tx2)) (l2))) ((fun r_ => (let_body) (r_)) (l2))) (v_)
| Term_TypeLambda v_ => (fun (tl2 : TypeLambda) => ((termHeadType) (((extendGraphForTypeLambda) (tx2)) (tl2))) ((fun r_ => (typeLambda_body) (r_)) (tl2))) (v_)
| Term_TypeApplication v_ => (fun (tat2 : TypeApplicationTerm) => ((maybes.bind) (((termHeadType) (tx2)) ((fun r_ => (typeApplicationTerm_body) (r_)) (tat2)))) (fun (htyp2 : Type_) => (fun x_ => match x_ with
| Type__Forall v_ => (fun (ft2 : ForallType) => (Some) ((((replaceFreeTypeVariable) ((fun r_ => (forallType_parameter) (r_)) (ft2))) ((fun r_ => (typeApplicationTerm_type) (r_)) (tat2))) ((fun r_ => (forallType_body) (r_)) (ft2)))) (v_)
| _ => (Some) (htyp2)
end) (htyp2))) (v_)
| Term_Variable v_ => (fun (vn2 : Name) => ((maybes.map) (typeSchemeToFType)) (((maps.lookup) (vn2)) ((fun r_ => (graph_boundTypes) (r_)) (tx2)))) (v_)
| _ => None
end) (trm2)) in let recurse := fun (tx1 : hydra.graph.Graph) => fun (term1 : Term) => (((rewriteWithArgs) (nil)) (tx1)) (term1) in let forMap := fun (mp : (list) ((prod) (Term) (Term))) => let forPair := fun (pr : (prod) (Term) (Term)) => (pair) (((recurse) (tx)) ((pairs.first) (pr))) (((recurse) (tx)) ((pairs.second) (pr))) in (maps.fromList) (((lists.map) (forPair)) ((maps.toList) (mp))) in let forField := fun (f : Field) => (Build_Field) ((fun r_ => (field_name) (r_)) (f)) (((recurse) (tx)) ((fun r_ => (field_term) (r_)) (f))) in let forCaseBranch := fun (f : Field) => let branchBody := ((recurse) (tx)) ((fun r_ => (field_term) (r_)) (f)) in let branchHType := ((termHeadType) (tx)) (branchBody) in let arty := ((termArityWithContext) (tx)) (branchBody) in (Build_Field) ((fun r_ => (field_name) (r_)) (f)) ((((((expand) (true)) (nil)) (arty)) (branchHType)) (branchBody)) in let afterRecursion := fun (trm : Term) => let hType := ((termHeadType) (tx)) (trm) in let arity := ((termArityWithContext) (tx)) (trm) in (((((expand) (false)) (args)) (arity)) (hType)) (trm) in (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (afterRecursion) ((Term_Annotated) ((Build_AnnotatedTerm) (((recurse) (tx)) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) ((fun r_ => (annotatedTerm_annotation) (r_)) (at_))))) (v_)
| Term_Application v_ => (fun (app : Application) => let rhs := (((rewriteWithArgs) (nil)) (tx)) ((fun r_ => (application_argument) (r_)) (app)) in (((rewriteWithArgs) (((lists.cons) (rhs)) (args))) (tx)) ((fun r_ => (application_function) (r_)) (app))) (v_)
| Term_Either v_ => (fun (e : (sum) (Term) (Term)) => (afterRecursion) ((Term_Either) ((((eithers.either) (fun (l : Term) => (inl) (((recurse) (tx)) (l)))) (fun (r : Term) => (inr) (((recurse) (tx)) (r)))) (e)))) (v_)
| Term_Cases v_ => (fun (cs : CaseStatement) => let newCs := (Build_CaseStatement) ((fun r_ => (caseStatement_typeName) (r_)) (cs)) (((maybes.map) (fun (t1 : Term) => ((recurse) (tx)) (t1))) ((fun r_ => (caseStatement_default) (r_)) (cs))) (((lists.map) (forCaseBranch)) ((fun r_ => (caseStatement_cases) (r_)) (cs))) in let elimTerm := (Term_Cases) (newCs) in let elimHeadType := (Some) ((Type__Function) ((Build_FunctionType) ((Type__Variable) ((fun r_ => (caseStatement_typeName) (r_)) (cs))) ((Type__Unit) (tt)))) in (((((expand) (true)) (args)) ((1)%Z)) (elimHeadType)) (elimTerm)) (v_)
| Term_Project v_ => (fun (p : Projection) => (((((expand) (false)) (args)) ((1)%Z)) (None)) ((Term_Project) (p))) (v_)
| Term_Unwrap v_ => (fun (nm : Name) => (((((expand) (false)) (args)) ((1)%Z)) (None)) ((Term_Unwrap) (nm))) (v_)
| Term_Lambda v_ => (fun (lm : Lambda) => let tx1 := ((extendGraphForLambda) (tx)) (lm) in let body := (((rewriteWithArgs) (nil)) (tx1)) ((fun r_ => (lambda_body) (r_)) (lm)) in let result := (Term_Lambda) ((Build_Lambda) ((fun r_ => (lambda_parameter) (r_)) (lm)) ((fun r_ => (lambda_domain) (r_)) (lm)) (body)) in let arty := ((termArityWithContext) (tx)) (result) in (((((expand) (false)) (args)) (arty)) (None)) (result)) (v_)
| Term_Let v_ => (fun (lt : Let) => let tx1 := (((extendGraphForLet) (fun (_ : hydra.graph.Graph) => fun (_2 : Binding) => None)) (tx)) (lt) in let mapBinding := fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) ((((rewriteWithArgs) (nil)) (tx1)) ((fun r_ => (binding_term) (r_)) (b))) ((fun r_ => (binding_type) (r_)) (b)) in let result := (Term_Let) ((Build_Let) (((lists.map) (mapBinding)) ((fun r_ => (let_bindings) (r_)) (lt))) ((((rewriteWithArgs) (nil)) (tx1)) ((fun r_ => (let_body) (r_)) (lt)))) in (afterRecursion) (result)) (v_)
| Term_List v_ => (fun (els : (list) (Term)) => (afterRecursion) ((Term_List) (((lists.map) (fun (el : Term) => ((recurse) (tx)) (el))) (els)))) (v_)
| Term_Literal v_ => (fun (v : Literal) => (Term_Literal) (v)) (v_)
| Term_Map v_ => (fun (mp : (list) ((prod) (Term) (Term))) => (afterRecursion) ((Term_Map) ((forMap) (mp)))) (v_)
| Term_Maybe v_ => (fun (mb : (option) (Term)) => (afterRecursion) ((Term_Maybe) (((maybes.map) (fun (v : Term) => ((recurse) (tx)) (v))) (mb)))) (v_)
| Term_Pair v_ => (fun (pr : (prod) (Term) (Term)) => (afterRecursion) ((Term_Pair) ((pair) (((recurse) (tx)) ((pairs.first) (pr))) (((recurse) (tx)) ((pairs.second) (pr)))))) (v_)
| Term_Record v_ => (fun (rc : Record_) => (afterRecursion) ((Term_Record) ((Build_Record_) ((fun r_ => (record__typeName) (r_)) (rc)) (((lists.map) (forField)) ((fun r_ => (record__fields) (r_)) (rc)))))) (v_)
| Term_Set v_ => (fun (st : (list) (Term)) => (afterRecursion) ((Term_Set) ((sets.fromList) (((lists.map) (fun (el : Term) => ((recurse) (tx)) (el))) ((sets.toList) (st)))))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => (afterRecursion) ((Term_TypeApplication) ((Build_TypeApplicationTerm) (((recurse) (tx)) ((fun r_ => (typeApplicationTerm_body) (r_)) (tt))) ((fun r_ => (typeApplicationTerm_type) (r_)) (tt))))) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => let tx1 := ((extendGraphForTypeLambda) (tx)) (tl) in let result := (Term_TypeLambda) ((Build_TypeLambda) ((fun r_ => (typeLambda_parameter) (r_)) (tl)) ((((rewriteWithArgs) (nil)) (tx1)) ((fun r_ => (typeLambda_body) (r_)) (tl)))) in (afterRecursion) (result)) (v_)
| Term_Inject v_ => (fun (inj : Injection) => (afterRecursion) ((Term_Inject) ((Build_Injection) ((fun r_ => (injection_typeName) (r_)) (inj)) ((forField) ((fun r_ => (injection_field) (r_)) (inj)))))) (v_)
| Term_Unit _ => (Term_Unit) (tt)
| Term_Variable v_ => (fun (vn : Name) => let varType := ((maybes.map) (typeSchemeToFType)) (((maps.lookup) (vn)) ((fun r_ => (graph_boundTypes) (r_)) (tx))) in let arty := ((termArityWithContext) (tx)) (term_) in (((((expand) (false)) (args)) (arty)) (varType)) (term_)) (v_)
| Term_Wrap v_ => (fun (wt : WrappedTerm) => (afterRecursion) ((Term_Wrap) ((Build_WrappedTerm) ((fun r_ => (wrappedTerm_typeName) (r_)) (wt)) (((recurse) (tx)) ((fun r_ => (wrappedTerm_body) (r_)) (wt)))))) (v_)
end) (term_)) in (contractTerm) ((((rewriteWithArgs) (nil)) (tx0)) (term0)).
Definition betaReduceType_bundle (t0 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Type_) , (sum) (Error) (Type_)) =>
    let betaReduceType := bundle_ in
    fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (typ : Type_) => let reduceApp := (hydra_fix) (fun reduceApp => fun (app : ApplicationType) => let rhs := (fun r_ => (applicationType_argument) (r_)) (app) in let lhs := (fun r_ => (applicationType_function) (r_)) (app) in (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => ((eithers.bind) ((reduceApp) ((Build_ApplicationType) ((fun r_ => (annotatedType_body) (r_)) (at_)) (rhs)))) (fun (a : Type_) => (inr) ((Type__Annotated) ((Build_AnnotatedType) (a) ((fun r_ => (annotatedType_annotation) (r_)) (at_)))))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => (((betaReduceType) (cx)) (graph_)) ((((replaceFreeTypeVariable) ((fun r_ => (forallType_parameter) (r_)) (ft))) (rhs)) ((fun r_ => (forallType_body) (r_)) (ft)))) (v_)
| Type__Variable v_ => (fun (name : Name) => ((eithers.bind) ((((requireType) (cx)) (graph_)) (name))) (fun (t' : Type_) => (((betaReduceType) (cx)) (graph_)) ((Type__Application) ((Build_ApplicationType) (t') (rhs))))) (v_)
| _ => hydra_unreachable
end) (lhs)) in let mapExpr := fun (t1 : Type) => fun (recurse : forall (_ : t1) , (sum) (Error) (Type_)) => fun (t : t1) => let findApp := fun (r : Type_) => (fun x_ => match x_ with
| Type__Application v_ => (fun (a : ApplicationType) => (reduceApp) (a)) (v_)
| _ => (inr) (r)
end) (r) in ((eithers.bind) ((recurse) (t))) (fun (r : Type_) => (findApp) (r)) in ((rewriteTypeM) ((mapExpr) (Type_))) (typ)).
Arguments betaReduceType_bundle {t0}.

Definition betaReduceType (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Type_) , (sum) (Error) (Type_) :=
  betaReduceType_bundle.
Arguments betaReduceType {t0}.
Definition alphaConvert : forall (_ : Name) , forall (_ : Name) , forall (_ : Term) , Term := fun (vold : Name) => fun (vnew : Name) => fun (term_ : Term) => (((replaceFreeTermVariable) (vold)) ((Term_Variable) (vnew))) (term_).

