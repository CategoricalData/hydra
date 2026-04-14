(* Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.paths hydra.ast hydra.classes hydra.coders hydra.context hydra.core hydra.error.checking hydra.error.core hydra.error.packaging hydra.errors hydra.graph hydra.json.model hydra.packaging hydra.parsing hydra.phantoms hydra.query hydra.relational hydra.tabular hydra.testing hydra.topology hydra.typing hydra.util hydra.variants hydra.lexical hydra.rewriting hydra.environment hydra.resolution hydra.scoping hydra.sorting hydra.strip hydra.substitution hydra.variables.

Axiom augmentBindingsWithNewFreeVars : forall (_ : hydra.graph.Graph) , forall (_ : (list) (hydra.core.Name)) , forall (_ : (list) (hydra.core.Binding)) , (prod) ((list) (hydra.core.Binding)) (hydra.typing.TermSubst).

Axiom bindingIsPolymorphic : forall (_ : hydra.core.Binding) , bool.

Axiom bindingUsesContextTypeVars : forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Binding) , bool.

Axiom countVarOccurrences : forall (_ : hydra.core.Name) , forall (_ : hydra.core.Term) , Z.

Axiom hoistAllLetBindings : forall (_ : hydra.core.Let) , hydra.core.Let.

Axiom hoistCaseStatements : forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Term) , hydra.core.Term.

Axiom hoistCaseStatementsInGraph : forall (_ : (list) (hydra.core.Binding)) , (list) (hydra.core.Binding).

Axiom hoistLetBindingsWithContext : forall (_ : forall (_ : hydra.core.Binding) , bool) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Let) , hydra.core.Let.

Axiom hoistLetBindingsWithPredicate : forall (_ : forall (_ : hydra.core.Binding) , bool) , forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Binding) , bool) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Let) , hydra.core.Let.

Axiom hoistPolymorphicLetBindings : forall (_ : forall (_ : hydra.core.Binding) , bool) , forall (_ : hydra.core.Let) , hydra.core.Let.

Axiom hoistSubterms : forall (_ : forall (_ : (prod) ((list) (hydra.paths.SubtermStep)) (hydra.core.Term)) , bool) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Term) , hydra.core.Term.

Axiom isApplicationFunction : forall (_ : hydra.paths.SubtermStep) , bool.

Axiom isLambdaBody : forall (_ : hydra.paths.SubtermStep) , bool.

Axiom isUnionElimination : forall (_ : hydra.core.Term) , bool.

Axiom isUnionEliminationApplication : forall (_ : hydra.core.Term) , bool.

Axiom normalizePathForHoisting : forall (_ : (list) (hydra.paths.SubtermStep)) , (list) (hydra.paths.SubtermStep).

Axiom shouldHoistAll : forall (_ : t0) , forall (_ : t1) , bool.

Axiom shouldHoistCaseStatement : forall (_ : (prod) ((list) (hydra.paths.SubtermStep)) (hydra.core.Term)) , bool.

Axiom shouldHoistPolymorphic : forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Binding) , bool.

Axiom updateHoistState : forall (_ : hydra.paths.SubtermStep) , forall (_ : (prod) (bool) (bool)) , (prod) (bool) (bool).
