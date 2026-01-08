-- | A module for creating graphs of dependencies between Hydra terms and types

{-
Usage example:

:m
import Hydra.Kernel
import Hydra.Sources.Kernel.Types.Core
import Hydra.Sources.Kernel.Types.All
import Hydra.Sources.Kernel.Terms.All
import qualified Hydra.Json.Writer as JsonWriter
import qualified Data.List as L
import Hydra.Tools.Monads
import Hydra.Ext.Tools.Analysis.Dependencies
import Hydra.Generation (modulesToGraph)
import System.IO

let jsonValuesToString = L.intercalate "\n" . fmap JsonWriter.printJson
termModulesToGraphson withPrims modules outFile = flowToIo hydraCoreGraph (jsonValuesToString <$> termGraphToDependencyGraphson withPrims False (modulesToGraph modules)) >>= writeFile outFile
typeModulesToGraphson modules outFile = flowToIo hydraCoreGraph (jsonValuesToString <$> typeGraphToDependencyGraphson (modulesToGraph modules)) >>= writeFile outFile
combinedModulesToGraphson dataModules schemaModules outFile = flowToIo hydraCoreGraph (jsonValuesToString <$> combinedGraphToDependencyGraphson (modulesToGraph dataModules) (modulesToGraph schemaModules)) >>= writeFile outFile

typeModulesToGraphson [module_] "/tmp/hydra-core-deps.json"
typeModulesToGraphson kernelTypesModules "/tmp/kernel-types-deps.json"
termModulesToGraphson True kernelTermsModules "/tmp/kernel-terms-deps-withPrims.json"
termModulesToGraphson False kernelTermsModules "/tmp/kernel-terms-deps-noPrims.json"
combinedModulesToGraphson kernelTermsModules kernelTypesModules "/tmp/kernel-combined-deps.json"

Now in G.V():

// Clear
g.V().drop()
// Choose one of the following
g.io("/tmp/hydra-core-deps.json").read().iterate()
g.io("/tmp/kernel-types-deps.json").read().iterate()
g.io("/tmp/kernel-terms-deps-withPrims.json").read().iterate()
g.io("/tmp/kernel-terms-deps-noPrims.json").read().iterate()
g.io("/tmp/kernel-combined-deps.json").read().iterate()
// Display
g.E()
-}
module Hydra.Ext.Tools.Analysis.Dependencies where

import Hydra.Kernel
import Hydra.Generation
import Hydra.Sources.Kernel.Terms.All
import Hydra.Sources.Libraries
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Meta as ShowMeta
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Util as Util
import Hydra.Ext.Staging.Pg.Utils
import Hydra.Ext.Staging.Pg.Graphson.Utils

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.Set as S
import qualified Data.Maybe as Y


--

vertexLabel_Primitive = PG.VertexLabel "Primitive"
vertexLabel_Term = PG.VertexLabel "Term"
vertexLabel_Type = PG.VertexLabel "Type"
edgeLabel_hasType = PG.EdgeLabel "hasType"
edgeLabel_subterm = PG.EdgeLabel "subterm"
edgeLabel_subtype = PG.EdgeLabel "subtype"
edgeLabel_usesPrimitive = PG.EdgeLabel "usesPrimitive"
propertyKey_arity = PG.PropertyKey "arity"
propertyKey_localName = PG.PropertyKey "localName"
propertyKey_name = PG.PropertyKey "name"
propertyKey_namespace = PG.PropertyKey "namespace"
propertyKey_termExpression = PG.PropertyKey "termExpression"
propertyKey_termVariant = PG.PropertyKey "termVariant"
propertyKey_typeExpression = PG.PropertyKey "typeExpression"
propertyKey_typeVariant = PG.PropertyKey "typeVariant"

--

combinedGraphToDependencyGraphson :: Graph -> Graph -> Flow Graph [Json.Value]
combinedGraphToDependencyGraphson dataGraph schemaGraph = do
  pg <- combinedGraphToDependencyPropertyGraph dataGraph schemaGraph
  pgElementsToGraphson stringGraphsonContext $ propertyGraphElements pg

combinedGraphToDependencyPropertyGraph :: Graph -> Graph -> Flow Graph (PG.Graph String)
combinedGraphToDependencyPropertyGraph dataGraph schemaGraph = do
  let pgDataGraph = termGraphToDependencyPropertyGraph True True dataGraph
  pgSchemaGraph <- typeGraphToDependencyPropertyGraph schemaGraph
  return $ PG.Graph
    (M.union (PG.graphVertices pgDataGraph) (PG.graphVertices pgSchemaGraph))
    (M.union (PG.graphEdges pgDataGraph) (PG.graphEdges pgSchemaGraph))

descriptiveEdgeId :: PG.EdgeLabel -> String -> String -> String
descriptiveEdgeId label outId inId = outId ++ "_" ++ PG.unEdgeLabel label ++ "_" ++ inId

namePairToEdge :: PG.EdgeLabel -> Name -> Name -> PG.Edge String
namePairToEdge label outName inName = PG.Edge label edgeId outId inId M.empty
  where
    outId = nameToVertexId outName
    inId = nameToVertexId inName
    edgeId = descriptiveEdgeId label outId inId

nameToNamespace :: Name -> String
nameToNamespace name = Y.maybe "default" unNamespace $ namespaceOf name

nameToVertexId :: Name -> String
nameToVertexId = unName

termGraphToDependencyGraphson :: Bool -> Bool -> Graph -> Flow s [Json.Value]
termGraphToDependencyGraphson withPrims withTypes g = pgElementsToGraphson stringGraphsonContext $
  propertyGraphElements $ termGraphToDependencyPropertyGraph withPrims withTypes g

-- | Given a Hydra graph, create a property graph in which the vertices are all elements of the graph
--   (plus all primitives, if selected) and the edges are all direct dependencies between elements
--   (and between elements and primitives, if selected).
--   Each vertex has an id based on the name of the element or primitive, in addition to a "namespace" property.
termGraphToDependencyPropertyGraph :: Bool -> Bool -> Graph -> PG.Graph String
termGraphToDependencyPropertyGraph withPrims withTypes g = PG.Graph vertexMap edgeMap
  where
    primitives = graphPrimitives g
    elements = graphElements g
    nameToVertexId = unName
    vertexMap = M.fromList $ fmap (\v -> (PG.vertexId v, v)) vertices
      where
        vertices = prims ++ els
          where
            prims = if withPrims
                then fmap toVertex $ M.elems primitives
                else []
              where
                toVertex prim = PG.Vertex vertexLabel_Primitive (nameToVertexId $ primitiveName prim) $ M.fromList [
                    (propertyKey_arity, show $ primitiveArity prim),
                    (propertyKey_localName, localNameOf name),
                    (propertyKey_name, unName name),
                    (propertyKey_namespace, nameToNamespace name),
                    (propertyKey_typeExpression, ShowCore.typeScheme $ primitiveType prim)]
                  where
                    name = primitiveName prim
            els = fmap toVertex $ M.elems elements
              where
                toVertex el = PG.Vertex vertexLabel_Term (nameToVertexId $ bindingName el) $ M.fromList [
                    (propertyKey_localName, localNameOf name),
                    (propertyKey_name, unName name),
                    (propertyKey_namespace, nameToNamespace name),
                    (propertyKey_termExpression, ShowCore.term term),
                    (propertyKey_termVariant, ShowMeta.termVariant $ termVariant term)]
                  where
                    name = bindingName el
                    term = bindingTerm el
    edgeMap = M.fromList $ fmap (\e -> (PG.edgeId e, e)) edges
      where
        edges = typeEdges ++ primEdges ++ elEdges
          where
            typeEdges = if withTypes
                then L.concat $ fmap edgesFrom elements
                else []
              where
                edgesFrom el = fmap (namePairToEdge edgeLabel_hasType (bindingName el)) $
                  S.toList $ termDependencyNames False False True $ bindingTerm el
            primEdges = if withPrims
                then L.concat $ fmap edgesFrom elements
                else []
              where
                edgesFrom el = fmap (namePairToEdge edgeLabel_usesPrimitive (bindingName el)) $
                  S.toList $ termDependencyNames False True False $ bindingTerm el
            elEdges = L.concat $ fmap edgesFrom elements
              where
                edgesFrom el = fmap (namePairToEdge edgeLabel_subterm (bindingName el)) $
                  S.toList $ freeVariablesInTerm $ bindingTerm el

typeGraphToDependencyGraphson :: Graph -> Flow Graph [Json.Value]
typeGraphToDependencyGraphson g = do
  pg <- typeGraphToDependencyPropertyGraph g
  pgElementsToGraphson stringGraphsonContext $ propertyGraphElements pg

typeGraphToDependencyPropertyGraph :: Graph -> Flow Graph (PG.Graph String)
typeGraphToDependencyPropertyGraph g = do
    types <- CM.mapM (\t -> Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ g t) terms
    let vertices = L.zipWith toVertex names types
    let edges = L.concat $ L.zipWith toEdges names $ fmap (S.toList . freeVariablesInType) types
    return $ PG.Graph
      (M.fromList $ fmap (\v -> (PG.vertexId v, v)) vertices)
      (M.fromList $ fmap (\e -> (PG.edgeId e, e)) edges)
  where
    elements = M.elems $ graphElements g
    terms = fmap bindingTerm elements
    names = fmap bindingName elements
    toEdges name deps = fmap (namePairToEdge edgeLabel_subtype name) deps
    toVertex name typ = PG.Vertex vertexLabel_Type (nameToVertexId name) $
      M.fromList [
        (propertyKey_localName, localNameOf name),
        (propertyKey_name, unName name),
        (propertyKey_namespace, nameToNamespace name),
        (propertyKey_typeExpression, ShowCore.type_ typ),
        (propertyKey_typeVariant, ShowMeta.typeVariant $ typeVariant typ)]
