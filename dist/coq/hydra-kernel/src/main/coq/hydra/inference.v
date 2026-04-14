(* Type inference following Algorithm W, extended for nominal terms and types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.context hydra.core hydra.typing hydra.substitution hydra.errors hydra.lib.eithers hydra.annotations hydra.lib.strings hydra.show.core hydra.show.typing hydra.lib.maps hydra.lib.lists hydra.lib.pairs hydra.lib.maybes hydra.lib.sets hydra.graph hydra.unification hydra.checking hydra.reflect hydra.names hydra.variables hydra.lib.logic hydra.lib.equality hydra.resolution hydra.extract.core hydra.sorting hydra.lib.math hydra.lib.literals hydra.rewriting hydra.lexical.

Axiom yieldWithConstraints : Context_ -> Term -> Type_ -> TypeSubst -> (list) ((prod) (Name) (TypeVariableMetadata)) -> InferenceResult.
Axiom yieldDebug : forall (t0 : Type), Context_ -> t0 -> string -> Term -> Type_ -> TypeSubst -> (sum) (Error) (InferenceResult).
Axiom yieldCheckedWithConstraints : Context_ -> Term -> Type_ -> TypeSubst -> (list) ((prod) (Name) (TypeVariableMetadata)) -> InferenceResult.
Axiom yieldChecked : Context_ -> Term -> Type_ -> TypeSubst -> InferenceResult.
Axiom yield : Context_ -> Term -> Type_ -> TypeSubst -> InferenceResult.
Axiom showInferenceResult : InferenceResult -> string.
Axiom mergeClassConstraints : forall (t0 : Type), (list) ((prod) (t0) (TypeVariableMetadata)) -> (list) ((prod) (t0) (TypeVariableMetadata)) -> (list) ((prod) (t0) (TypeVariableMetadata)).
Axiom mapConstraints : forall (t0 : Type) (t1 : Type), t0 -> hydra.graph.Graph -> (TypeSubst -> t1) -> (list) (TypeConstraint) -> (sum) (Error) (t1).
Axiom inferTypeOfUnit : Context_ -> InferenceResult.
Axiom inferTypeOfLiteral : Context_ -> Literal -> InferenceResult.
Axiom freshVariableType : Context_ -> (prod) (Type_) (Context_).
Axiom freeVariablesInContext : hydra.graph.Graph -> (list) (Name).
Axiom isUnbound : hydra.graph.Graph -> Name -> bool.
Axiom generalize : hydra.graph.Graph -> Type_ -> TypeScheme.
Axiom extendContext : (list) ((prod) (Name) (TypeScheme)) -> hydra.graph.Graph -> hydra.graph.Graph.
Axiom buildTypeApplicationTerm : (list) (Name) -> Term -> Term.
Axiom inferTypeOfPrimitive : Context_ -> hydra.graph.Graph -> Name -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfProjection : Context_ -> hydra.graph.Graph -> Projection -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfUnwrap : Context_ -> hydra.graph.Graph -> Name -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfVariable : Context_ -> hydra.graph.Graph -> Name -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfWrappedTerm_inferTypeOfTerm_bundle : unit.

Axiom inferTypeOfWrappedTerm : Context_ -> hydra.graph.Graph -> WrappedTerm -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfTerm : Context_ -> hydra.graph.Graph -> Term -> string -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfAnnotatedTerm : Context_ -> hydra.graph.Graph -> AnnotatedTerm -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfApplication : Context_ -> hydra.graph.Graph -> Application -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfCaseStatement : Context_ -> hydra.graph.Graph -> CaseStatement -> (sum) (Error) (InferenceResult).
Axiom inferMany : Context_ -> hydra.graph.Graph -> (list) ((prod) (Term) (string)) -> (sum) (Error) ((prod) ((prod) ((list) (Term)) ((prod) ((list) (Type_)) ((prod) (TypeSubst) ((list) ((prod) (Name) (TypeVariableMetadata)))))) (Context_)).
Axiom inferTypeOfEither : Context_ -> hydra.graph.Graph -> (sum) (Term) (Term) -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfInjection : Context_ -> hydra.graph.Graph -> Injection -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfLambda : Context_ -> hydra.graph.Graph -> Lambda -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfLet : Context_ -> hydra.graph.Graph -> Let -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfLetNormalized : Context_ -> hydra.graph.Graph -> Let -> (sum) (Error) (InferenceResult).
Axiom inferTypesOfTemporaryBindings : Context_ -> hydra.graph.Graph -> (list) (Binding) -> (sum) (Error) ((prod) ((prod) ((list) (Term)) ((prod) ((list) (Type_)) ((prod) (TypeSubst) ((list) ((prod) (Name) (TypeVariableMetadata)))))) (Context_)).
Axiom inferTypeOfList : Context_ -> hydra.graph.Graph -> (list) (Term) -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfCollection : Context_ -> hydra.graph.Graph -> (Type_ -> Type_) -> ((list) (Term) -> Term) -> string -> (list) (Name) -> (list) (Term) -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfMap : Context_ -> hydra.graph.Graph -> (list) ((prod) (Term) (Term)) -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfOptional : Context_ -> hydra.graph.Graph -> (option) (Term) -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfPair : Context_ -> hydra.graph.Graph -> (prod) (Term) (Term) -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfRecord : Context_ -> hydra.graph.Graph -> Record_ -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfSet : Context_ -> hydra.graph.Graph -> (list) (Term) -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfTypeApplication : Context_ -> hydra.graph.Graph -> TypeApplicationTerm -> (sum) (Error) (InferenceResult).
Axiom inferTypeOfTypeLambda : Context_ -> hydra.graph.Graph -> TypeLambda -> (sum) (Error) (InferenceResult).
Axiom forInferredTerm : forall (t0 : Type), Context_ -> hydra.graph.Graph -> Term -> string -> (InferenceResult -> t0) -> (sum) (Error) ((prod) (t0) (Context_)).
Axiom inferInGraphContext : Context_ -> hydra.graph.Graph -> Term -> (sum) (Error) (InferenceResult).
Axiom bindUnboundTypeVariables_bundle : unit.

Axiom bindUnboundTypeVariables : hydra.graph.Graph -> Term -> Term.
Axiom finalizeInferredTerm : forall (t0 : Type), t0 -> hydra.graph.Graph -> Term -> (sum) (Error) (Term).
Axiom inferGraphTypes : Context_ -> (list) (Binding) -> hydra.graph.Graph -> (sum) (Error) ((prod) ((prod) (hydra.graph.Graph) ((list) (Binding))) (Context_)).
Axiom inferTypeOf : Context_ -> hydra.graph.Graph -> Term -> (sum) (Error) ((prod) ((prod) (Term) (TypeScheme)) (Context_)).
Axiom bindConstraints : forall (t0 : Type), t0 -> hydra.graph.Graph -> (list) (TypeConstraint) -> (sum) (Error) (TypeSubst).

