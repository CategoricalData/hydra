(* Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.paths hydra.lib.pairs hydra.lib.logic hydra.core hydra.strip hydra.lib.lists hydra.graph hydra.lib.maps hydra.scoping hydra.lib.strings hydra.lib.literals hydra.variables hydra.lib.sets hydra.lexical hydra.lib.math hydra.rewriting hydra.lib.maybes hydra.environment hydra.lib.equality hydra.typing hydra.resolution hydra.sorting hydra.substitution.

Axiom updateHoistState : SubtermStep -> (prod) (bool) (bool) -> (prod) (bool) (bool).
Axiom shouldHoistAll : forall (t0 : Type) (t1 : Type), t0 -> t1 -> bool.
Axiom isLambdaBody : SubtermStep -> bool.
Axiom isEliminationUnion : Function -> bool.
Axiom isUnionElimination : Term -> bool.
Axiom isUnionEliminationApplication : Term -> bool.
Axiom shouldHoistCaseStatement : (prod) ((list) (SubtermStep)) (Term) -> bool.
Axiom isApplicationFunction : SubtermStep -> bool.
Axiom normalizePathForHoisting : (list) (SubtermStep) -> (list) (SubtermStep).
Axiom hoistSubterms : ((prod) ((list) (SubtermStep)) (Term) -> bool) -> hydra.graph.Graph -> Term -> Term.
Axiom hoistCaseStatements : hydra.graph.Graph -> Term -> Term.
Axiom hoistCaseStatementsInGraph : (list) (Binding) -> (list) (Binding).
Axiom countVarOccurrences_bundle : unit.

Axiom countVarOccurrences : Name -> Term -> Z.
Axiom bindingUsesContextTypeVars : hydra.graph.Graph -> Binding -> bool.
Axiom bindingIsPolymorphic : Binding -> bool.
Axiom shouldHoistPolymorphic : hydra.graph.Graph -> Binding -> bool.
Axiom augmentBindingsWithNewFreeVars : hydra.graph.Graph -> (list) (Name) -> (list) (Binding) -> (prod) ((list) (Binding)) (TermSubst).
Axiom hoistLetBindingsWithPredicate : (Binding -> bool) -> (hydra.graph.Graph -> Binding -> bool) -> hydra.graph.Graph -> Let -> Let.
Axiom hoistAllLetBindings : Let -> Let.
Axiom hoistLetBindingsWithContext : (Binding -> bool) -> hydra.graph.Graph -> Let -> Let.
Axiom hoistPolymorphicLetBindings : (Binding -> bool) -> Let -> Let.

