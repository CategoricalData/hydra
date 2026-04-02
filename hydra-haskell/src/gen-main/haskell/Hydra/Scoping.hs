-- Note: this is an automatically generated file. Do not edit.

-- | Graph context extension and type scheme conversion

module Hydra.Scoping where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Extend a graph by descending into a lambda body
extendGraphForLambda :: Graph.Graph -> Core.Lambda -> Graph.Graph
extendGraphForLambda g lam =

      let var = Core.lambdaParameter lam
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms g),
        Graph.graphBoundTypes = (Maybes.maybe (Graph.graphBoundTypes g) (\dom -> Maps.insert var (fTypeToTypeScheme dom) (Graph.graphBoundTypes g)) (Core.lambdaDomain lam)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Sets.insert var (Graph.graphLambdaVariables g)),
        Graph.graphMetadata = (Maps.delete var (Graph.graphMetadata g)),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Extend a graph by descending into a let body
extendGraphForLet :: (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> Graph.Graph -> Core.Let -> Graph.Graph
extendGraphForLet forBinding g letrec =

      let bindings = Core.letBindings letrec
          g2 = extendGraphWithBindings bindings g
      in Graph.Graph {
        Graph.graphBoundTerms = (Maps.union (Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) bindings)) (Graph.graphBoundTerms g)),
        Graph.graphBoundTypes = (Maps.union (Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) bindings))) (Graph.graphBoundTypes g)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Lists.foldl (\s -> \b -> Sets.delete (Core.bindingName b) s) (Graph.graphLambdaVariables g) bindings),
        Graph.graphMetadata = (Graph.graphMetadata (Lists.foldl (\gAcc -> \b ->
          let m = Graph.graphMetadata gAcc
              newMeta = Maybes.maybe (Maps.delete (Core.bindingName b) m) (\t -> Maps.insert (Core.bindingName b) t m) (forBinding gAcc b)
          in Graph.Graph {
            Graph.graphBoundTerms = (Graph.graphBoundTerms gAcc),
            Graph.graphBoundTypes = (Graph.graphBoundTypes gAcc),
            Graph.graphClassConstraints = (Graph.graphClassConstraints gAcc),
            Graph.graphLambdaVariables = (Graph.graphLambdaVariables gAcc),
            Graph.graphMetadata = newMeta,
            Graph.graphPrimitives = (Graph.graphPrimitives gAcc),
            Graph.graphSchemaTypes = (Graph.graphSchemaTypes gAcc),
            Graph.graphTypeVariables = (Graph.graphTypeVariables gAcc)}) g2 bindings)),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Extend a graph by descending into a type lambda body
extendGraphForTypeLambda :: Graph.Graph -> Core.TypeLambda -> Graph.Graph
extendGraphForTypeLambda g tlam =

      let name = Core.typeLambdaParameter tlam
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms g),
        Graph.graphBoundTypes = (Graph.graphBoundTypes g),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
        Graph.graphMetadata = (Graph.graphMetadata g),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Sets.insert name (Graph.graphTypeVariables g))}

-- | Add bindings to an existing graph
extendGraphWithBindings :: [Core.Binding] -> Graph.Graph -> Graph.Graph
extendGraphWithBindings bindings g =

      let newTerms = Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) bindings)
          newTypes =
                  Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) bindings))
      in Graph.Graph {
        Graph.graphBoundTerms = (Maps.union newTerms (Graph.graphBoundTerms g)),
        Graph.graphBoundTypes = (Maps.union newTypes (Graph.graphBoundTypes g)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
        Graph.graphMetadata = (Graph.graphMetadata g),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Convert a forall type to a type scheme
fTypeToTypeScheme :: Core.Type -> Core.TypeScheme
fTypeToTypeScheme typ =

      let stripAnnotations =
              \t -> case t of
                Core.TypeAnnotated v0 -> stripAnnotations (Core.annotatedTypeBody v0)
                _ -> t
          gatherForall =
                  \vars -> \typ2 -> case (stripAnnotations typ2) of
                    Core.TypeForall v0 -> gatherForall (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0)
                    _ -> Core.TypeScheme {
                      Core.typeSchemeVariables = (Lists.reverse vars),
                      Core.typeSchemeType = typ2,
                      Core.typeSchemeConstraints = Nothing}
      in (gatherForall [] typ)

-- | Convert a type scheme to a forall type
typeSchemeToFType :: Core.TypeScheme -> Core.Type
typeSchemeToFType ts =

      let vars = Core.typeSchemeVariables ts
          body = Core.typeSchemeType ts
      in (Lists.foldl (\t -> \v -> Core.TypeForall (Core.ForallType {
        Core.forallTypeParameter = v,
        Core.forallTypeBody = t})) body (Lists.reverse vars))
