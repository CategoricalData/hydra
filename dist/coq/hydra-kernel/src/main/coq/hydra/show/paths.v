(* Utilities for working with subterm steps and paths. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.lists hydra.lib.maps hydra.lib.maybes hydra.lib.pairs hydra.lib.sets hydra.lib.strings hydra.names hydra.packaging hydra.paths hydra.rewriting.

Definition subtermStep : forall (_ : SubtermStep) , (option) (string) := fun (step : SubtermStep) => let idx := fun i => None in let idxSuff := fun (suffix : string) => fun i => ((maybes.map) (fun (s : string) => ((strings.cat2) (s)) (suffix))) ((idx) (i)) in (fun x_ => match x_ with
| SubtermStep_AnnotatedBody _ => (None) : (option) (string)
| SubtermStep_ApplicationFunction _ => (Some) ("fun"%string)
| SubtermStep_ApplicationArgument _ => (Some) ("arg"%string)
| SubtermStep_LambdaBody _ => (Some) ("body"%string)
| SubtermStep_UnionCasesDefault _ => (Some) ("default"%string)
| SubtermStep_UnionCasesBranch v_ => (fun (name : Name) => (Some) (((strings.cat2) ("."%string)) ((fun w_ => w_) (name)))) (v_)
| SubtermStep_LetBody _ => (Some) ("in"%string)
| SubtermStep_LetBinding v_ => (fun (name : Name) => (Some) (((strings.cat2) ((fun w_ => w_) (name))) ("="%string))) (v_)
| SubtermStep_ListElement v_ => (fun (i : Z) => (idx) (i)) (v_)
| SubtermStep_MapKey v_ => (fun (i : Z) => ((idxSuff) (".key"%string)) (i)) (v_)
| SubtermStep_MapValue v_ => (fun (i : Z) => ((idxSuff) (".value"%string)) (i)) (v_)
| SubtermStep_MaybeTerm _ => (Some) ("just"%string)
| SubtermStep_ProductTerm v_ => (fun (i : Z) => (idx) (i)) (v_)
| SubtermStep_RecordField v_ => (fun (name : Name) => (Some) (((strings.cat2) ("."%string)) ((fun w_ => w_) (name)))) (v_)
| SubtermStep_SetElement v_ => (fun (i : Z) => (idx) (i)) (v_)
| SubtermStep_SumTerm _ => (None) : (option) (string)
| SubtermStep_TypeLambdaBody _ => (None) : (option) (string)
| SubtermStep_TypeApplicationTerm _ => (None) : (option) (string)
| SubtermStep_InjectionTerm _ => (None) : (option) (string)
| SubtermStep_WrappedTerm _ => (None) : (option) (string)
end) (step).
Definition termToSubtermGraph : forall (_ : (list) ((prod) (Namespace) (string))) , forall (_ : Term) , SubtermGraph := fun (namespaces : (list) ((prod) (Namespace) (string))) => fun (term_ : Term) => let dontCareStep := (SubtermStep_AnnotatedBody) (tt) in let helper := (hydra_fix) (fun helper => fun (ids : (list) ((prod) (Name) (SubtermNode))) => fun (mroot : (option) (SubtermNode)) => fun (path : (list) (SubtermStep)) => fun (state : (prod) ((prod) ((list) (SubtermNode)) ((list) (SubtermEdge))) ((list) (string))) => fun (stepTerm : (prod) (SubtermStep) (Term)) => let currentTerm := (pairs.second) (stepTerm) in let nodesEdges := (pairs.first) (state) in let edges := (pairs.second) (nodesEdges) in let step := (pairs.first) (stepTerm) in let nextPath := ((lists.cons) (step)) (path) in let nodes := (pairs.first) (nodesEdges) in let visited := (pairs.second) (state) in (fun x_ => match x_ with
| Term_Let v_ => (fun (letExpr : Let) => let addBindingName := fun (nodesVisitedIds : (prod) ((prod) ((list) (SubtermNode)) ((list) (string))) ((list) ((prod) (Name) (SubtermNode)))) => fun (name : Name) => let currentIds := (pairs.second) (nodesVisitedIds) in let currentNodesVisited := (pairs.first) (nodesVisitedIds) in let currentNodes := (pairs.first) (currentNodesVisited) in let currentVisited := (pairs.second) (currentNodesVisited) in let rawLabel := ((compactName) (namespaces)) (name) in let uniqueLabel := ((uniqueLabel) (currentVisited)) (rawLabel) in let node := (Build_SubtermNode) (name) (rawLabel) (uniqueLabel) in let newIds := (((maps.insert) (name)) (node)) (currentIds) in let newNodes := ((lists.cons) (node)) (currentNodes) in let newVisited := ((sets.insert) (uniqueLabel)) (currentVisited) in (pair) ((pair) (newNodes) (newVisited)) (newIds) in let bindings := (fun r_ => (let_bindings) (r_)) (letExpr) in let bindingNames := ((lists.map) (fun r_ => (binding_name) (r_))) (bindings) in let nodesVisitedIds1 := (((lists.foldl) (addBindingName)) ((pair) ((pair) (nil) (visited)) (ids))) (bindingNames) in let ids1 := (pairs.second) (nodesVisitedIds1) in let addBindingTerm := fun (currentState : (prod) ((prod) ((list) (SubtermNode)) ((list) (SubtermEdge))) ((list) (string))) => fun (nodeBinding : (prod) (SubtermNode) (Binding)) => let binding := (pairs.second) (nodeBinding) in let root := (pairs.first) (nodeBinding) in let term1 := (fun r_ => (binding_term) (r_)) (binding) in (((((helper) (ids1)) ((Some) (root))) (nil)) (currentState)) ((pair) (dontCareStep) (term1)) in let env := (fun r_ => (let_body) (r_)) (letExpr) in let nodes1 := (pairs.first) ((pairs.first) (nodesVisitedIds1)) in let nodeBindingPairs := ((lists.zip) (nodes1)) (bindings) in let visited1 := (pairs.second) ((pairs.first) (nodesVisitedIds1)) in let stateAfterBindings := (((lists.foldl) (addBindingTerm)) ((pair) ((pair) (((lists.concat2) (nodes1)) (nodes)) (edges)) (visited1))) (nodeBindingPairs) in (((((helper) (ids1)) (mroot)) (nextPath)) (stateAfterBindings)) ((pair) ((SubtermStep_LetBody) (tt)) (env))) (v_)
| Term_Variable v_ => (fun (name : Name) => (((maybes.maybe) (state)) (fun (root : SubtermNode) => (((maybes.maybe) (state)) (fun (node : SubtermNode) => let edge := (Build_SubtermEdge) (root) ((lists.reverse) (nextPath)) (node) in let newEdges := ((lists.cons) (edge)) (edges) in (pair) ((pair) (nodes) (newEdges)) (visited))) (((maps.lookup) (name)) (ids)))) (mroot)) (v_)
| _ => (((lists.foldl) ((((helper) (ids)) (mroot)) (nextPath))) (state)) ((subtermsWithSteps) (currentTerm))
end) (currentTerm)) in let initialState := (pair) ((pair) (nil) (nil)) (sets.empty) in let result := (((((helper) (maps.empty)) ((None) : (option) (SubtermNode))) (nil)) (initialState)) ((pair) (dontCareStep) (term_)) in let finalNodesEdges := (pairs.first) (result) in let finalEdges := (pairs.second) (finalNodesEdges) in let finalNodes := (pairs.first) (finalNodesEdges) in (Build_SubtermGraph) (finalNodes) (finalEdges).

