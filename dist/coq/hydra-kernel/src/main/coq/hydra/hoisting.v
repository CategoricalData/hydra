(* Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.paths hydra.ast hydra.classes hydra.coders hydra.context hydra.core hydra.error.checking hydra.error.core hydra.error.packaging hydra.errors hydra.graph hydra.json.model hydra.packaging hydra.parsing hydra.phantoms hydra.query hydra.relational hydra.tabular hydra.testing hydra.topology hydra.typing hydra.util hydra.variants hydra.lexical hydra.rewriting hydra.environment hydra.resolution hydra.scoping hydra.sorting hydra.strip hydra.substitution hydra.variables.

Axiom augmentBindingsWithNewFreeVars : forall (_ : hydra.graph.Graph) , forall (_ : (list) (Name)) , forall (_ : (list) (Binding)) , (prod) ((list) (Binding)) (TermSubst).

Axiom bindingIsPolymorphic : forall (_ : Binding) , bool.

Axiom bindingUsesContextTypeVars : forall (_ : hydra.graph.Graph) , forall (_ : Binding) , bool.

Axiom countVarOccurrences : forall (_ : Name) , forall (_ : Term) , Z.

Axiom hoistAllLetBindings : forall (_ : Let) , Let.

Axiom hoistCaseStatements : forall (_ : hydra.graph.Graph) , forall (_ : Term) , Term.

Axiom hoistCaseStatementsInGraph : forall (_ : (list) (Binding)) , (list) (Binding).

Axiom hoistLetBindingsWithContext : forall (_ : forall (_ : Binding) , bool) , forall (_ : hydra.graph.Graph) , forall (_ : Let) , Let.

Axiom hoistLetBindingsWithPredicate : forall (_ : forall (_ : Binding) , bool) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Binding) , bool) , forall (_ : hydra.graph.Graph) , forall (_ : Let) , Let.

Axiom hoistPolymorphicLetBindings : forall (_ : forall (_ : Binding) , bool) , forall (_ : Let) , Let.

Axiom hoistSubterms : forall (_ : forall (_ : (prod) ((list) (SubtermStep)) (Term)) , bool) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , Term.

Axiom isApplicationFunction : forall (_ : SubtermStep) , bool.

Axiom isLambdaBody : forall (_ : SubtermStep) , bool.

Axiom isUnionElimination : forall (_ : Term) , bool.

Axiom isUnionEliminationApplication : forall (_ : Term) , bool.

Axiom normalizePathForHoisting : forall (_ : (list) (SubtermStep)) , (list) (SubtermStep).

Axiom shouldHoistAll : forall (t0 : Type) , forall (t1 : Type) , forall (_ : t0) , forall (_ : t1) , bool.

Axiom shouldHoistCaseStatement : forall (_ : (prod) ((list) (SubtermStep)) (Term)) , bool.

Axiom shouldHoistPolymorphic : forall (_ : hydra.graph.Graph) , forall (_ : Binding) , bool.

Axiom updateHoistState : forall (_ : SubtermStep) , forall (_ : (prod) (bool) (bool)) , (prod) (bool) (bool).
