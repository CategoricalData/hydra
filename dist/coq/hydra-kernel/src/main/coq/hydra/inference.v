(* Type inference following Algorithm W, extended for nominal terms and types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.paths hydra.ast hydra.classes hydra.coders hydra.context hydra.core hydra.error.checking hydra.error.core hydra.error.packaging hydra.errors hydra.graph hydra.json.model hydra.packaging hydra.parsing hydra.phantoms hydra.query hydra.relational hydra.tabular hydra.testing hydra.topology hydra.typing hydra.util hydra.variants hydra.annotations hydra.checking hydra.extract.core hydra.lexical hydra.reflect hydra.rewriting hydra.names hydra.resolution hydra.show.core hydra.show.errors hydra.show.typing hydra.sorting hydra.substitution hydra.variables hydra.unification.

Axiom bindConstraints : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (hydra.typing.TypeConstraint)) , (sum) (hydra.errors.Error) (hydra.typing.TypeSubst).

Axiom bindUnboundTypeVariables : forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Term) , hydra.core.Term.

Axiom buildTypeApplicationTerm : forall (_ : (list) (hydra.core.Name)) , forall (_ : hydra.core.Term) , hydra.core.Term.

Axiom extendContext : forall (_ : (list) ((prod) (hydra.core.Name) (hydra.core.TypeScheme))) , forall (_ : hydra.graph.Graph) , hydra.graph.Graph.

Axiom finalizeInferredTerm : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Term) , (sum) (hydra.errors.Error) (hydra.core.Term).

Axiom forInferredTerm : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Term) , forall (_ : string) , forall (_ : forall (_ : hydra.typing.InferenceResult) , t0) , (sum) (hydra.errors.Error) ((prod) (t0) (hydra.context.Context)).

Axiom freeVariablesInContext : forall (_ : hydra.graph.Graph) , (list) (hydra.core.Name).

Axiom freshVariableType : forall (_ : hydra.context.Context) , (prod) (hydra.core.Type) (hydra.context.Context).

Axiom generalize : forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Type) , hydra.core.TypeScheme.

Axiom inferGraphTypes : forall (_ : hydra.context.Context) , forall (_ : (list) (hydra.core.Binding)) , forall (_ : hydra.graph.Graph) , (sum) (hydra.errors.Error) ((prod) ((prod) (hydra.graph.Graph) ((list) (hydra.core.Binding))) (hydra.context.Context)).

Axiom inferInGraphContext : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Term) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferMany : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : (list) ((prod) (hydra.core.Term) (string))) , (sum) (hydra.errors.Error) ((prod) ((prod) ((list) (hydra.core.Term)) ((prod) ((list) (hydra.core.Type)) ((prod) (hydra.typing.TypeSubst) ((list) ((prod) (hydra.core.Name) (hydra.core.TypeVariableMetadata)))))) (hydra.context.Context)).

Axiom inferTypeOf : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Term) , (sum) (hydra.errors.Error) ((prod) ((prod) (hydra.core.Term) (hydra.core.TypeScheme)) (hydra.context.Context)).

Axiom inferTypeOfAnnotatedTerm : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.AnnotatedTerm) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfApplication : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Application) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfCaseStatement : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.CaseStatement) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfCollection : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : forall (_ : hydra.core.Type) , hydra.core.Type) , forall (_ : forall (_ : (list) (hydra.core.Term)) , hydra.core.Term) , forall (_ : string) , forall (_ : (list) (hydra.core.Name)) , forall (_ : (list) (hydra.core.Term)) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfEither : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : (sum) (hydra.core.Term) (hydra.core.Term)) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfInjection : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Injection) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfLambda : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Lambda) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfLet : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Let) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfLetNormalized : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Let) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfList : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (hydra.core.Term)) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfLiteral : forall (_ : hydra.context.Context) , forall (_ : hydra.core.Literal) , hydra.typing.InferenceResult.

Axiom inferTypeOfMap : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : (list) ((prod) (hydra.core.Term) (hydra.core.Term))) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfOptional : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : (option) (hydra.core.Term)) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfPair : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : (prod) (hydra.core.Term) (hydra.core.Term)) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfPrimitive : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Name) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfProjection : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Projection) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfRecord : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Record) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfSet : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (hydra.core.Term)) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfTerm : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Term) , forall (_ : string) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfTypeApplication : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.TypeApplicationTerm) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfTypeLambda : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.TypeLambda) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfUnit : forall (_ : hydra.context.Context) , hydra.typing.InferenceResult.

Axiom inferTypeOfUnwrap : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Name) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfVariable : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Name) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypeOfWrappedTerm : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.WrappedTerm) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom inferTypesOfTemporaryBindings : forall (_ : hydra.context.Context) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (hydra.core.Binding)) , (sum) (hydra.errors.Error) ((prod) ((prod) ((list) (hydra.core.Term)) ((prod) ((list) (hydra.core.Type)) ((prod) (hydra.typing.TypeSubst) ((list) ((prod) (hydra.core.Name) (hydra.core.TypeVariableMetadata)))))) (hydra.context.Context)).

Axiom isUnbound : forall (_ : hydra.graph.Graph) , forall (_ : hydra.core.Name) , bool.

Axiom mapConstraints : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : forall (_ : hydra.typing.TypeSubst) , t1) , forall (_ : (list) (hydra.typing.TypeConstraint)) , (sum) (hydra.errors.Error) (t1).

Axiom mergeClassConstraints : forall (_ : (list) ((prod) (t0) (hydra.core.TypeVariableMetadata))) , forall (_ : (list) ((prod) (t0) (hydra.core.TypeVariableMetadata))) , (list) ((prod) (t0) (hydra.core.TypeVariableMetadata)).

Axiom showInferenceResult : forall (_ : hydra.typing.InferenceResult) , string.

Axiom yield : forall (_ : hydra.context.Context) , forall (_ : hydra.core.Term) , forall (_ : hydra.core.Type) , forall (_ : hydra.typing.TypeSubst) , hydra.typing.InferenceResult.

Axiom yieldChecked : forall (_ : hydra.context.Context) , forall (_ : hydra.core.Term) , forall (_ : hydra.core.Type) , forall (_ : hydra.typing.TypeSubst) , hydra.typing.InferenceResult.

Axiom yieldCheckedWithConstraints : forall (_ : hydra.context.Context) , forall (_ : hydra.core.Term) , forall (_ : hydra.core.Type) , forall (_ : hydra.typing.TypeSubst) , forall (_ : (list) ((prod) (hydra.core.Name) (hydra.core.TypeVariableMetadata))) , hydra.typing.InferenceResult.

Axiom yieldDebug : forall (_ : hydra.context.Context) , forall (_ : t0) , forall (_ : string) , forall (_ : hydra.core.Term) , forall (_ : hydra.core.Type) , forall (_ : hydra.typing.TypeSubst) , (sum) (hydra.errors.Error) (hydra.typing.InferenceResult).

Axiom yieldWithConstraints : forall (_ : hydra.context.Context) , forall (_ : hydra.core.Term) , forall (_ : hydra.core.Type) , forall (_ : hydra.typing.TypeSubst) , forall (_ : (list) ((prod) (hydra.core.Name) (hydra.core.TypeVariableMetadata))) , hydra.typing.InferenceResult.
