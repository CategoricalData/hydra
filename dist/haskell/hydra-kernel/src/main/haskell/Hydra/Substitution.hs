-- Note: this is an automatically generated file. Do not edit.

-- | Variable substitution in type and term expressions.

module Hydra.Substitution where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Typing as Typing
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | Compose two type substitutions
composeTypeSubst :: Typing.TypeSubst -> Typing.TypeSubst -> Typing.TypeSubst
composeTypeSubst s1 s2 =
    Logic.ifElse (Maps.null (Typing.unTypeSubst s1)) s2 (Logic.ifElse (Maps.null (Typing.unTypeSubst s2)) s1 (composeTypeSubstNonEmpty s1 s2))

-- | Compose a list of type substitutions
composeTypeSubstList :: [Typing.TypeSubst] -> Typing.TypeSubst
composeTypeSubstList = Lists.foldl composeTypeSubst idTypeSubst

-- | Compose two non-empty type substitutions (internal helper)
composeTypeSubstNonEmpty :: Typing.TypeSubst -> Typing.TypeSubst -> Typing.TypeSubst
composeTypeSubstNonEmpty s1 s2 =

      let isExtra = \k -> \v -> Maybes.isNothing (Maps.lookup k (Typing.unTypeSubst s1))
          withExtra = Maps.filterWithKey isExtra (Typing.unTypeSubst s2)
      in (Typing.TypeSubst (Maps.union withExtra (Maps.map (substInType s2) (Typing.unTypeSubst s1))))

-- | The identity type substitution
idTypeSubst :: Typing.TypeSubst
idTypeSubst = Typing.TypeSubst Maps.empty

-- | Create a type substitution with a single variable mapping
singletonTypeSubst :: Core.Name -> Core.Type -> Typing.TypeSubst
singletonTypeSubst v t = Typing.TypeSubst (Maps.singleton v t)

-- | Apply a type substitution to class constraints, propagating to free variables
substInClassConstraints :: Typing.TypeSubst -> M.Map Core.Name Core.TypeVariableMetadata -> M.Map Core.Name Core.TypeVariableMetadata
substInClassConstraints subst constraints =

      let substMap = Typing.unTypeSubst subst
          insertOrMerge =
                  \varName -> \metadata -> \acc -> Maybes.maybe (Maps.insert varName metadata acc) (\existing ->
                    let merged =
                            Core.TypeVariableMetadata {
                              Core.typeVariableMetadataClasses = (Sets.union (Core.typeVariableMetadataClasses existing) (Core.typeVariableMetadataClasses metadata))}
                    in (Maps.insert varName merged acc)) (Maps.lookup varName acc)
      in (Lists.foldl (\acc -> \pair ->
        let varName = Pairs.first pair
            metadata = Pairs.second pair
        in (Maybes.maybe (insertOrMerge varName metadata acc) (\targetType ->
          let freeVars = Sets.toList (Variables.freeVariablesInType targetType)
          in (Lists.foldl (\acc2 -> \freeVar -> insertOrMerge freeVar metadata acc2) acc freeVars)) (Maps.lookup varName substMap))) Maps.empty (Maps.toList constraints))

-- | Apply a type substitution to a graph's bound types and class constraints
substInContext :: Typing.TypeSubst -> Graph.Graph -> Graph.Graph
substInContext subst cx =

      let newBoundTypes = Maps.map (substInTypeScheme subst) (Graph.graphBoundTypes cx)
          newClassConstraints = substInClassConstraints subst (Graph.graphClassConstraints cx)
          cx2 =
                  Graph.Graph {
                    Graph.graphBoundTerms = (Graph.graphBoundTerms cx),
                    Graph.graphBoundTypes = newBoundTypes,
                    Graph.graphClassConstraints = (Graph.graphClassConstraints cx),
                    Graph.graphLambdaVariables = (Graph.graphLambdaVariables cx),
                    Graph.graphMetadata = (Graph.graphMetadata cx),
                    Graph.graphPrimitives = (Graph.graphPrimitives cx),
                    Graph.graphSchemaTypes = (Graph.graphSchemaTypes cx),
                    Graph.graphTypeVariables = (Graph.graphTypeVariables cx)}
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms cx2),
        Graph.graphBoundTypes = (Graph.graphBoundTypes cx2),
        Graph.graphClassConstraints = newClassConstraints,
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables cx2),
        Graph.graphMetadata = (Graph.graphMetadata cx2),
        Graph.graphPrimitives = (Graph.graphPrimitives cx2),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes cx2),
        Graph.graphTypeVariables = (Graph.graphTypeVariables cx2)}

-- | Apply a type substitution to a type
substInType :: Typing.TypeSubst -> Core.Type -> Core.Type
substInType subst typ0 = Logic.ifElse (Maps.null (Typing.unTypeSubst subst)) typ0 (substInTypeNonEmpty subst typ0)

-- | Apply a non-empty type substitution to a type (internal helper)
substInTypeNonEmpty :: Typing.TypeSubst -> Core.Type -> Core.Type
substInTypeNonEmpty subst typ0 =

      let rewrite =
              \recurse -> \typ -> case typ of
                Core.TypeForall v0 -> Maybes.maybe (recurse typ) (\styp -> Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.forallTypeParameter v0),
                  Core.forallTypeBody = (substInType (removeVar (Core.forallTypeParameter v0)) (Core.forallTypeBody v0))})) (Maps.lookup (Core.forallTypeParameter v0) (Typing.unTypeSubst subst))
                Core.TypeVariable v0 -> Maybes.maybe typ (\styp -> styp) (Maps.lookup v0 (Typing.unTypeSubst subst))
                _ -> recurse typ
          removeVar = \v -> Typing.TypeSubst (Maps.delete v (Typing.unTypeSubst subst))
      in (Rewriting.rewriteType rewrite typ0)

-- | Apply a type substitution to a type scheme. The scheme's quantifier variables shadow the substitution: any name in typeSchemeVariables is removed from subst before substituting into the body and constraints. Without this, a substitution like {t0 -> Foo} applied to `forall [t0]. t0 -> t0` would incorrectly replace the bound t0.
substInTypeScheme :: Typing.TypeSubst -> Core.TypeScheme -> Core.TypeScheme
substInTypeScheme subst ts =

      let scopedSubst =
              Typing.TypeSubst (Lists.foldl (\m -> \v -> Maps.delete v m) (Typing.unTypeSubst subst) (Core.typeSchemeVariables ts))
      in Core.TypeScheme {
        Core.typeSchemeVariables = (Core.typeSchemeVariables ts),
        Core.typeSchemeType = (substInType scopedSubst (Core.typeSchemeType ts)),
        Core.typeSchemeConstraints = (Maybes.map (substInClassConstraints scopedSubst) (Core.typeSchemeConstraints ts))}

-- | Apply a type substitution to the type annotations within a term
substTypesInTerm :: Typing.TypeSubst -> Core.Term -> Core.Term
substTypesInTerm subst term0 =

      let rewrite =
              \recurse -> \term ->
                let dflt = recurse term
                    forLambda =
                            \l -> Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.lambdaParameter l),
                              Core.lambdaDomain = (Maybes.map (substInType subst) (Core.lambdaDomain l)),
                              Core.lambdaBody = (substTypesInTerm subst (Core.lambdaBody l))})
                    forLet =
                            \l ->
                              let rewriteBinding =
                                      \b -> Core.Binding {
                                        Core.bindingName = (Core.bindingName b),
                                        Core.bindingTerm = (substTypesInTerm subst (Core.bindingTerm b)),
                                        Core.bindingType = (Maybes.map (substInTypeScheme subst) (Core.bindingType b))}
                              in (Core.TermLet (Core.Let {
                                Core.letBindings = (Lists.map rewriteBinding (Core.letBindings l)),
                                Core.letBody = (substTypesInTerm subst (Core.letBody l))}))
                    forTypeApplication =
                            \tt -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = (substTypesInTerm subst (Core.typeApplicationTermBody tt)),
                              Core.typeApplicationTermType = (substInType subst (Core.typeApplicationTermType tt))})
                    forTypeLambda =
                            \ta ->
                              let param = Core.typeLambdaParameter ta
                                  subst2 = Typing.TypeSubst (Maps.delete param (Typing.unTypeSubst subst))
                              in (Core.TermTypeLambda (Core.TypeLambda {
                                Core.typeLambdaParameter = param,
                                Core.typeLambdaBody = (substTypesInTerm subst2 (Core.typeLambdaBody ta))}))
                in case term of
                  Core.TermLambda v0 -> forLambda v0
                  Core.TermLet v0 -> forLet v0
                  Core.TermTypeApplication v0 -> forTypeApplication v0
                  Core.TermTypeLambda v0 -> forTypeLambda v0
                  _ -> dflt
      in (Rewriting.rewriteTerm rewrite term0)

-- | Apply a term substitution to a binding
substituteInBinding :: Typing.TermSubst -> Core.Binding -> Core.Binding
substituteInBinding subst b =
    Core.Binding {
      Core.bindingName = (Core.bindingName b),
      Core.bindingTerm = (substituteInTerm subst (Core.bindingTerm b)),
      Core.bindingType = (Core.bindingType b)}

-- | Apply a type substitution to a type constraint
substituteInConstraint :: Typing.TypeSubst -> Typing.TypeConstraint -> Typing.TypeConstraint
substituteInConstraint subst c =
    Typing.TypeConstraint {
      Typing.typeConstraintLeft = (substInType subst (Typing.typeConstraintLeft c)),
      Typing.typeConstraintRight = (substInType subst (Typing.typeConstraintRight c)),
      Typing.typeConstraintComment = (Typing.typeConstraintComment c)}

-- | Apply a type substitution to a list of type constraints
substituteInConstraints :: Typing.TypeSubst -> [Typing.TypeConstraint] -> [Typing.TypeConstraint]
substituteInConstraints subst cs = Lists.map (substituteInConstraint subst) cs

-- | Apply a term substitution to a term
substituteInTerm :: Typing.TermSubst -> Core.Term -> Core.Term
substituteInTerm subst term0 =

      let s = Typing.unTermSubst subst
          rewrite =
                  \recurse -> \term ->
                    let withLambda =
                            \l ->
                              let v = Core.lambdaParameter l
                                  subst2 = Typing.TermSubst (Maps.delete v s)
                              in (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = v,
                                Core.lambdaDomain = (Core.lambdaDomain l),
                                Core.lambdaBody = (substituteInTerm subst2 (Core.lambdaBody l))}))
                        withLet =
                                \lt ->
                                  let bindings = Core.letBindings lt
                                      names = Sets.fromList (Lists.map Core.bindingName bindings)
                                      subst2 = Typing.TermSubst (Maps.filterWithKey (\k -> \v -> Logic.not (Sets.member k names)) s)
                                      rewriteBinding =
                                              \b -> Core.Binding {
                                                Core.bindingName = (Core.bindingName b),
                                                Core.bindingTerm = (substituteInTerm subst2 (Core.bindingTerm b)),
                                                Core.bindingType = (Core.bindingType b)}
                                  in (Core.TermLet (Core.Let {
                                    Core.letBindings = (Lists.map rewriteBinding bindings),
                                    Core.letBody = (substituteInTerm subst2 (Core.letBody lt))}))
                    in case term of
                      Core.TermLambda v0 -> withLambda v0
                      Core.TermLet v0 -> withLet v0
                      Core.TermVariable v0 -> Maybes.maybe (recurse term) (\sterm -> sterm) (Maps.lookup v0 s)
                      _ -> recurse term
      in (Rewriting.rewriteTerm rewrite term0)
