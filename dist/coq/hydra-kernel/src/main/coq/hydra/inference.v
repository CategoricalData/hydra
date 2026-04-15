(* Type inference following Algorithm W, extended for nominal terms and types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.paths hydra.ast hydra.classes hydra.coders hydra.context hydra.core hydra.error.checking hydra.error.core hydra.error.packaging hydra.errors hydra.graph hydra.json.model hydra.packaging hydra.parsing hydra.phantoms hydra.query hydra.relational hydra.tabular hydra.testing hydra.topology hydra.typing hydra.util hydra.variants hydra.annotations hydra.checking hydra.extract.core hydra.lexical hydra.reflect hydra.rewriting hydra.names hydra.resolution hydra.show.core hydra.show.errors hydra.show.typing hydra.sorting hydra.substitution hydra.variables hydra.unification.

Axiom bindConstraints : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (TypeConstraint)) , (sum) (Error) (TypeSubst).

Axiom bindUnboundTypeVariables : forall (_ : hydra.graph.Graph) , forall (_ : Term) , Term.

Axiom buildTypeApplicationTerm : forall (_ : (list) (Name)) , forall (_ : Term) , Term.

Axiom extendContext : forall (_ : (list) ((prod) (Name) (TypeScheme))) , forall (_ : hydra.graph.Graph) , hydra.graph.Graph.

Axiom finalizeInferredTerm : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Term).

Axiom forInferredTerm : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , forall (_ : string) , forall (_ : forall (_ : InferenceResult) , t0) , (sum) (Error) ((prod) (t0) (Context_)).

Axiom freeVariablesInContext : forall (_ : hydra.graph.Graph) , (list) (Name).

Axiom freshVariableType : forall (_ : Context_) , (prod) (Type_) (Context_).

Axiom generalize : forall (_ : hydra.graph.Graph) , forall (_ : Type_) , TypeScheme.

Axiom inferGraphTypes : forall (_ : Context_) , forall (_ : (list) (Binding)) , forall (_ : hydra.graph.Graph) , (sum) (Error) ((prod) ((prod) (hydra.graph.Graph) ((list) (Binding))) (Context_)).

Axiom inferInGraphContext : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (InferenceResult).

Axiom inferMany : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (list) ((prod) (Term) (string))) , (sum) (Error) ((prod) ((prod) ((list) (Term)) ((prod) ((list) (Type_)) ((prod) (TypeSubst) ((list) ((prod) (Name) (TypeVariableMetadata)))))) (Context_)).

Axiom inferTypeOf : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((prod) ((prod) (Term) (TypeScheme)) (Context_)).

Axiom inferTypeOfAnnotatedTerm : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : AnnotatedTerm) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfApplication : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Application) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfCaseStatement : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : CaseStatement) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfCollection : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : forall (_ : Type_) , Type_) , forall (_ : forall (_ : (list) (Term)) , Term) , forall (_ : string) , forall (_ : (list) (Name)) , forall (_ : (list) (Term)) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfEither : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (sum) (Term) (Term)) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfInjection : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Injection) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfLambda : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Lambda) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfLet : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Let) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfLetNormalized : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Let) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfList : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (Term)) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfLiteral : forall (_ : Context_) , forall (_ : Literal) , InferenceResult.

Axiom inferTypeOfMap : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (list) ((prod) (Term) (Term))) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfOptional : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (option) (Term)) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfPair : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (prod) (Term) (Term)) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfPrimitive : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfProjection : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Projection) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfRecord : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Record_) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfSet : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (Term)) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfTerm : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , forall (_ : string) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfTypeApplication : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : TypeApplicationTerm) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfTypeLambda : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : TypeLambda) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfUnit : forall (_ : Context_) , InferenceResult.

Axiom inferTypeOfUnwrap : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfVariable : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : Name) , (sum) (Error) (InferenceResult).

Axiom inferTypeOfWrappedTerm : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : WrappedTerm) , (sum) (Error) (InferenceResult).

Axiom inferTypesOfTemporaryBindings : forall (_ : Context_) , forall (_ : hydra.graph.Graph) , forall (_ : (list) (Binding)) , (sum) (Error) ((prod) ((prod) ((list) (Term)) ((prod) ((list) (Type_)) ((prod) (TypeSubst) ((list) ((prod) (Name) (TypeVariableMetadata)))))) (Context_)).

Axiom isUnbound : forall (_ : hydra.graph.Graph) , forall (_ : Name) , bool.

Axiom mapConstraints : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : forall (_ : TypeSubst) , t1) , forall (_ : (list) (TypeConstraint)) , (sum) (Error) (t1).

Axiom mergeClassConstraints : forall (_ : (list) ((prod) (t0) (TypeVariableMetadata))) , forall (_ : (list) ((prod) (t0) (TypeVariableMetadata))) , (list) ((prod) (t0) (TypeVariableMetadata)).

Axiom showInferenceResult : forall (_ : InferenceResult) , string.

Axiom yield : forall (_ : Context_) , forall (_ : Term) , forall (_ : Type_) , forall (_ : TypeSubst) , InferenceResult.

Axiom yieldChecked : forall (_ : Context_) , forall (_ : Term) , forall (_ : Type_) , forall (_ : TypeSubst) , InferenceResult.

Axiom yieldCheckedWithConstraints : forall (_ : Context_) , forall (_ : Term) , forall (_ : Type_) , forall (_ : TypeSubst) , forall (_ : (list) ((prod) (Name) (TypeVariableMetadata))) , InferenceResult.

Axiom yieldDebug : forall (_ : Context_) , forall (_ : t0) , forall (_ : string) , forall (_ : Term) , forall (_ : Type_) , forall (_ : TypeSubst) , (sum) (Error) (InferenceResult).

Axiom yieldWithConstraints : forall (_ : Context_) , forall (_ : Term) , forall (_ : Type_) , forall (_ : TypeSubst) , forall (_ : (list) ((prod) (Name) (TypeVariableMetadata))) , InferenceResult.
