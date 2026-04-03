-- Note: this is an automatically generated file. Do not edit.

-- | Graph to type environment conversions

module Hydra.Environment where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Module as Module
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Convert an element to a typed term
elementAsTypeApplicationTerm :: Context.Context -> Core.Binding -> Either (Context.InContext Errors.Error) Core.TypeApplicationTerm
elementAsTypeApplicationTerm cx el =
    Maybes.maybe (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "missing element type")),
      Context.inContextContext = cx})) (\ts -> Right (Core.TypeApplicationTerm {
      Core.typeApplicationTermBody = (Core.bindingTerm el),
      Core.typeApplicationTermType = (Core.typeSchemeType ts)})) (Core.bindingType el)

-- | Convert bindings and a body to a let expression
graphAsLet :: [Core.Binding] -> Core.Term -> Core.Let
graphAsLet bindings body =
    Core.Let {
      Core.letBindings = bindings,
      Core.letBody = body}

-- | Convert bindings and a body to a term, using let-term duality
graphAsTerm :: [Core.Binding] -> Core.Term -> Core.Term
graphAsTerm bindings body = Core.TermLet (graphAsLet bindings body)

-- | Decode a list of type-encoding bindings into a map of named types
graphAsTypes :: Context.Context -> Graph.Graph -> [Core.Binding] -> Either (Context.InContext Errors.DecodingError) (M.Map Core.Name Core.Type)
graphAsTypes cx graph els =

      let toPair =
              \el -> Eithers.map (\typ -> (Core.bindingName el, typ)) (Eithers.bimap (\_wc_e -> Context.InContext {
                Context.inContextObject = _wc_e,
                Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Core_.type_ graph (Core.bindingTerm el)))
      in (Eithers.map Maps.fromList (Eithers.mapList toPair els))

-- | Partition a list of definitions into type definitions and term definitions
partitionDefinitions :: [Module.Definition] -> ([Module.TypeDefinition], [Module.TermDefinition])
partitionDefinitions defs =

      let getType =
              \def -> case def of
                Module.DefinitionType v0 -> Just v0
                Module.DefinitionTerm _ -> Nothing
          getTerm =
                  \def -> case def of
                    Module.DefinitionType _ -> Nothing
                    Module.DefinitionTerm v0 -> Just v0
      in (Maybes.cat (Lists.map getType defs), (Maybes.cat (Lists.map getTerm defs)))

-- | Convert a schema graph to a typing environment (Either version)
schemaGraphToTypingEnvironment :: Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (M.Map Core.Name Core.TypeScheme)
schemaGraphToTypingEnvironment cx g =

      let toTypeScheme =
              \vars -> \typ -> case (Strip.deannotateType typ) of
                Core.TypeForall v0 -> toTypeScheme (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0)
                _ -> Core.TypeScheme {
                  Core.typeSchemeVariables = (Lists.reverse vars),
                  Core.typeSchemeType = typ,
                  Core.typeSchemeConstraints = Nothing}
          decodeType =
                  \term -> Eithers.bimap (\_wc_e -> Context.InContext {
                    Context.inContextObject = _wc_e,
                    Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.type_ g term))
          decodeTypeScheme =
                  \term -> Eithers.bimap (\_wc_e -> Context.InContext {
                    Context.inContextObject = _wc_e,
                    Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.typeScheme g term))
          toPair =
                  \el ->
                    let forTerm =
                            \term -> case term of
                              Core.TermRecord v0 -> Logic.ifElse (Equality.equal (Core.recordTypeName v0) (Core.Name "hydra.core.TypeScheme")) (Eithers.map Maybes.pure (decodeTypeScheme (Core.bindingTerm el))) (Right Nothing)
                              Core.TermUnion v0 -> Logic.ifElse (Equality.equal (Core.injectionTypeName v0) (Core.Name "hydra.core.Type")) (Eithers.map (\decoded -> Just (toTypeScheme [] decoded)) (decodeType (Core.bindingTerm el))) (Right Nothing)
                              _ -> Right Nothing
                    in (Eithers.bind (Maybes.maybe (Eithers.map (\typ -> Just (Scoping.fTypeToTypeScheme typ)) (decodeType (Core.bindingTerm el))) (\ts -> Logic.ifElse (Equality.equal ts (Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
                      Core.typeSchemeConstraints = Nothing})) (Eithers.map Maybes.pure (decodeTypeScheme (Core.bindingTerm el))) (Logic.ifElse (Equality.equal ts (Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                      Core.typeSchemeConstraints = Nothing})) (Eithers.map (\decoded -> Just (toTypeScheme [] decoded)) (decodeType (Core.bindingTerm el))) (forTerm (Strip.deannotateTerm (Core.bindingTerm el))))) (Core.bindingType el)) (\mts -> Right (Maybes.map (\ts -> (Core.bindingName el, ts)) mts)))
      in (Eithers.map (\mpairs -> Maps.fromList (Maybes.cat mpairs)) (Eithers.mapList toPair (Lexical.graphToBindings g)))

-- | Extract the bindings from a let term, or return an empty list for other terms
termAsBindings :: Core.Term -> [Core.Binding]
termAsBindings term =
    case (Strip.deannotateTerm term) of
      Core.TermLet v0 -> Core.letBindings v0
      _ -> []

-- | Encode a map of named types to a list of elements
typesToElements :: M.Map Core.Name Core.Type -> [Core.Binding]
typesToElements typeMap =

      let toElement =
              \pair ->
                let name = Pairs.first pair
                in Core.Binding {
                  Core.bindingName = name,
                  Core.bindingTerm = (Core__.type_ (Pairs.second pair)),
                  Core.bindingType = Nothing}
      in (Lists.map toElement (Maps.toList typeMap))

-- | Execute a computation in the context of a lambda body, extending the type context with the lambda parameter
withLambdaContext :: (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> t0 -> Core.Lambda -> (t1 -> t2) -> t2
withLambdaContext getContext setContext env lam body =

      let newContext = Scoping.extendGraphForLambda (getContext env) lam
      in (body (setContext newContext env))

-- | Execute a computation in the context of a let body, extending the type context with the let bindings
withLetContext :: (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> t0 -> Core.Let -> (t1 -> t2) -> t2
withLetContext getContext setContext forBinding env letrec body =

      let newContext = Scoping.extendGraphForLet forBinding (getContext env) letrec
      in (body (setContext newContext env))

-- | Execute a computation in the context of a type lambda body, extending the type context with the type parameter
withTypeLambdaContext :: (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> t0 -> Core.TypeLambda -> (t1 -> t2) -> t2
withTypeLambdaContext getContext setContext env tlam body =

      let newContext = Scoping.extendGraphForTypeLambda (getContext env) tlam
      in (body (setContext newContext env))
