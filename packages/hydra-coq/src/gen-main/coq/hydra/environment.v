(* Graph to type environment conversions *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.scoping hydra.lib.pairs hydra.encode.core hydra.lib.lists hydra.lib.maps hydra.strip hydra.errors hydra.lib.eithers hydra.decode.core hydra.lib.logic hydra.lib.equality hydra.lib.maybes hydra.lexical hydra.packaging hydra.sorting hydra.lib.sets hydra.variables.

Definition withTypeLambdaContext (t0 : Type) (t1 : Type) (t2 : Type) : (t0 -> hydra.graph.Graph) -> (hydra.graph.Graph -> t0 -> t1) -> t0 -> TypeLambda -> (t1 -> t2) -> t2 :=
  fun (getContext : t0 -> hydra.graph.Graph) => fun (setContext : hydra.graph.Graph -> t0 -> t1) => fun (env : t0) => fun (tlam : TypeLambda) => fun (body : t1 -> t2) => let newContext := ((extendGraphForTypeLambda) ((getContext) (env))) (tlam) in (body) (((setContext) (newContext)) (env)).
Arguments withTypeLambdaContext {t0} {t1} {t2}.
Definition withLetContext (t0 : Type) (t1 : Type) (t2 : Type) : (t0 -> hydra.graph.Graph) -> (hydra.graph.Graph -> t0 -> t1) -> (hydra.graph.Graph -> Binding -> (option) (Term)) -> t0 -> Let -> (t1 -> t2) -> t2 :=
  fun (getContext : t0 -> hydra.graph.Graph) => fun (setContext : hydra.graph.Graph -> t0 -> t1) => fun (forBinding : hydra.graph.Graph -> Binding -> (option) (Term)) => fun (env : t0) => fun (letrec : Let) => fun (body : t1 -> t2) => let newContext := (((extendGraphForLet) (forBinding)) ((getContext) (env))) (letrec) in (body) (((setContext) (newContext)) (env)).
Arguments withLetContext {t0} {t1} {t2}.
Definition withLambdaContext (t0 : Type) (t1 : Type) (t2 : Type) : (t0 -> hydra.graph.Graph) -> (hydra.graph.Graph -> t0 -> t1) -> t0 -> Lambda -> (t1 -> t2) -> t2 :=
  fun (getContext : t0 -> hydra.graph.Graph) => fun (setContext : hydra.graph.Graph -> t0 -> t1) => fun (env : t0) => fun (lam : Lambda) => fun (body : t1 -> t2) => let newContext := ((extendGraphForLambda) ((getContext) (env))) (lam) in (body) (((setContext) (newContext)) (env)).
Arguments withLambdaContext {t0} {t1} {t2}.
Definition typesToDefinitions : (list) ((prod) (Name) (Type_)) -> (list) (Binding) :=
  fun (typeMap : (list) ((prod) (Name) (Type_))) => let toElement := fun (pair_ : (prod) (Name) (Type_)) => let name := (pairs.first) (pair_) in (Build_Binding) (name) ((hydra.encode.core.type) ((pairs.second) (pair_))) (None) in ((lists.map) (toElement)) ((maps.toList) (typeMap)).
Definition termAsBindings : Term -> (list) (Binding) :=
  fun (term_ : Term) => (fun x_ => match x_ with
| Term_Let v_ => (fun (lt : Let) => (fun r_ => (let_bindings) (r_)) (lt)) (v_)
| _ => nil
end) ((deannotateTerm) (term_)).
Definition schemaGraphToTypingEnvironment : hydra.graph.Graph -> (sum) (Error) ((list) ((prod) (Name) (TypeScheme))) :=
  fun (g : hydra.graph.Graph) => let toTypeScheme := (hydra_fix) (fun toTypeScheme => fun (vars : (list) (Name)) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Forall v_ => (fun (ft : ForallType) => ((toTypeScheme) (((lists.cons) ((fun r_ => (forallType_parameter) (r_)) (ft))) (vars))) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| _ => (Build_TypeScheme) ((lists.reverse) (vars)) (typ) (None)
end) ((deannotateType) (typ))) in let decodeTypeScheme := fun (term_ : Term) => (((eithers.bimap) (fun (_e : DecodingError) => (Error_Decoding) (_e))) (fun (_a : TypeScheme) => _a)) (((hydra.decode.core.typeScheme) (g)) (term_)) in let decodeType := fun (term_ : Term) => (((eithers.bimap) (fun (_e : DecodingError) => (Error_Decoding) (_e))) (fun (_a : Type_) => _a)) (((hydra.decode.core.type) (g)) (term_)) in let toPair := fun (el : Binding) => let forTerm := fun (term_ : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (r : Record_) => (((logic.ifElse) (((equality.equal) ((fun r_ => (record__typeName) (r_)) (r))) ("TypeScheme"%string))) (((eithers.map) (maybes.pure)) ((decodeTypeScheme) ((fun r_ => (binding_term) (r_)) (el))))) ((inr) (None))) (v_)
| Term_Union v_ => (fun (i : Injection) => (((logic.ifElse) (((equality.equal) ((fun r_ => (injection_typeName) (r_)) (i))) ("Type_"%string))) (((eithers.map) (fun (decoded : Type_) => (Some) (((toTypeScheme) (nil)) (decoded)))) ((decodeType) ((fun r_ => (binding_term) (r_)) (el))))) ((inr) (None))) (v_)
| _ => (inr) (None)
end) (term_) in ((eithers.bind) ((((maybes.maybe) (((eithers.map) (fun (typ : Type_) => (Some) ((fTypeToTypeScheme) (typ)))) ((decodeType) ((fun r_ => (binding_term) (r_)) (el))))) (fun (ts : TypeScheme) => (((logic.ifElse) (((equality.equal) (ts)) ((Build_TypeScheme) (nil) ((Type__Variable) ("TypeScheme"%string)) (None)))) (((eithers.map) (maybes.pure)) ((decodeTypeScheme) ((fun r_ => (binding_term) (r_)) (el))))) ((((logic.ifElse) (((equality.equal) (ts)) ((Build_TypeScheme) (nil) ((Type__Variable) ("Type_"%string)) (None)))) (((eithers.map) (fun (decoded : Type_) => (Some) (((toTypeScheme) (nil)) (decoded)))) ((decodeType) ((fun r_ => (binding_term) (r_)) (el))))) ((forTerm) ((deannotateTerm) ((fun r_ => (binding_term) (r_)) (el))))))) ((fun r_ => (binding_type) (r_)) (el)))) (fun (mts : (option) (TypeScheme)) => (inr) (((maybes.map) (fun (ts : TypeScheme) => (pair) ((fun r_ => (binding_name) (r_)) (el)) (ts))) (mts))) in ((eithers.map) (fun (mpairs : (list) ((option) ((prod) (Name) (TypeScheme)))) => (maps.fromList) ((maybes.cat) (mpairs)))) (((eithers.mapList) (toPair)) ((graphToBindings) (g))).
Definition partitionDefinitions : (list) (Definition_) -> (prod) ((list) (TypeDefinition)) ((list) (TermDefinition)) :=
  fun (defs : (list) (Definition_)) => let getType := fun (def : Definition_) => (fun x_ => match x_ with
| Definition__Type v_ => (fun (td : TypeDefinition) => (Some) (td)) (v_)
| Definition__Term v_ => (fun (_ : TermDefinition) => None) (v_)
end) (def) in let getTerm := fun (def : Definition_) => (fun x_ => match x_ with
| Definition__Type v_ => (fun (_ : TypeDefinition) => None) (v_)
| Definition__Term v_ => (fun (td : TermDefinition) => (Some) (td)) (v_)
end) (def) in (pair) ((maybes.cat) (((lists.map) (getType)) (defs))) ((maybes.cat) (((lists.map) (getTerm)) (defs))).
Definition reorderDefs : (list) (Definition_) -> (list) (Definition_) :=
  fun (defs : (list) (Definition_)) => let partitioned := (partitionDefinitions) (defs) in let termDefsWrapped := ((lists.map) (fun (td : TermDefinition) => (Definition__Term) (td))) ((pairs.second) (partitioned)) in let sortedTermDefs := (lists.concat) ((((topologicalSortNodes) (fun (d : Definition_) => (fun x_ => match x_ with
| Definition__Term v_ => (fun (td : TermDefinition) => (fun r_ => (termDefinition_name) (r_)) (td)) (v_)
| _ => hydra_unreachable
end) (d))) (fun (d : Definition_) => (fun x_ => match x_ with
| Definition__Term v_ => (fun (td : TermDefinition) => (sets.toList) ((freeVariablesInTerm) ((fun r_ => (termDefinition_term) (r_)) (td)))) (v_)
| _ => nil
end) (d))) (termDefsWrapped)) in let typeDefsRaw := (pairs.first) (partitioned) in let nameRest := ((lists.filter) (fun (td : TypeDefinition) => (logic.not) (((equality.equal) ((fun r_ => (typeDefinition_name) (r_)) (td))) ("Name"%string)))) (typeDefsRaw) in let nameFirst := ((lists.filter) (fun (td : TypeDefinition) => ((equality.equal) ((fun r_ => (typeDefinition_name) (r_)) (td))) ("Name"%string))) (typeDefsRaw) in let typeDefs := (lists.concat) ((cons) (((lists.map) (fun (td : TypeDefinition) => (Definition__Type) (td))) (nameFirst)) ((cons) (((lists.map) (fun (td : TypeDefinition) => (Definition__Type) (td))) (nameRest)) (nil))) in (lists.concat) ((cons) (typeDefs) ((cons) (sortedTermDefs) (nil))).
Definition graphAsTypes : hydra.graph.Graph -> (list) (Binding) -> (sum) (DecodingError) ((list) ((prod) (Name) (Type_))) :=
  fun (graph_ : hydra.graph.Graph) => fun (els : (list) (Binding)) => let toPair := fun (el : Binding) => ((eithers.map) (fun (typ : Type_) => (pair) ((fun r_ => (binding_name) (r_)) (el)) (typ))) (((hydra.decode.core.type) (graph_)) ((fun r_ => (binding_term) (r_)) (el))) in ((eithers.map) (maps.fromList)) (((eithers.mapList) (toPair)) (els)).
Definition graphAsLet : (list) (Binding) -> Term -> Let :=
  fun (bindings : (list) (Binding)) => fun (body : Term) => (Build_Let) (bindings) (body).
Definition graphAsTerm : (list) (Binding) -> Term -> Term :=
  fun (bindings : (list) (Binding)) => fun (body : Term) => (Term_Let) (((graphAsLet) (bindings)) (body)).
Definition definitionAsTypeApplicationTerm : Binding -> (sum) (Error) (TypeApplicationTerm) :=
  fun (el : Binding) => (((maybes.maybe) ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("typed binding"%string) ("untyped binding"%string)))))) (fun (ts : TypeScheme) => (inr) ((Build_TypeApplicationTerm) ((fun r_ => (binding_term) (r_)) (el)) ((fun r_ => (typeScheme_type) (r_)) (ts))))) ((fun r_ => (binding_type) (r_)) (el)).

