-- | A module for creating textual summaries of Hydra graphs and programs

{-
:m
import Hydra.Kernel
import Hydra.Ext.Tools.Analysis.Summaries
import Hydra.Sources.Kernel.Types.Core
import Hydra.Tools.Monads
import Hydra.Generation
import Hydra.Sources.All

-- Create an exhaustive list of Hydra kernel elements and primitives for code generation
flowToIo hydraCoreGraph (graphSummary True $ modulesToGraph kernelModules) >>= putStrLn
-}
module Hydra.Ext.Tools.Analysis.Summaries where

import Hydra.Kernel
import Hydra.Generation
import Hydra.Ext.Sources.All
import Hydra.Sources.Libraries
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Describe.Core as DescribeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Pg.Model as PG

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.Set as S
import qualified Data.Maybe as Y


elementSummary :: Bool -> Element -> Flow Graph String
elementSummary withTypes el = do
    mt <- findTypeInfo
    return $ (unName $ elementName el) ++ Y.fromMaybe "" mt
  where
    findTypeInfo = if withTypes
      then case elementType el of
        Nothing -> return Nothing
        Just ts -> Just <$> if EncodeCore.isType (deannotateType $ typeSchemeType ts)
          then do
            typ <- deannotateType <$> (DecodeCore.type_ $ elementTerm el)
            return $ " = " ++ ShowCore.type_ typ
          else pure $ " : " ++ ShowCore.typeScheme ts
      else pure Nothing

elementsInModules :: [Module] -> [Element]
elementsInModules mods = L.sortBy (O.comparing elementName) $ L.concat $ fmap moduleElements mods

elementsInKernelModules :: [Element]
elementsInKernelModules = elementsInModules kernelModules

fieldSummary :: Field -> String
fieldSummary = unName . fieldName

fieldTypeSummary :: FieldType -> String
fieldTypeSummary (FieldType fname ftype) = unName fname ++ " : " ++ ShowCore.type_ ftype

graphSummary :: Bool -> Graph -> Flow Graph String
graphSummary withTypes g = do
  gi <- if withTypes then inferGraphTypes g else pure g
  let els = L.sortBy (O.comparing elementName) $ M.elems $ graphElements gi
  let prims = L.sortBy (O.comparing primitiveName) $ M.elems $ graphPrimitives gi
  elSummaries <- CM.mapM (elementSummary withTypes) els
  let primSummaries = fmap (primitiveSummary withTypes) prims
  return $ "Elements:\n" ++ showSummaries elSummaries ++ "\nPrimitives:\n" ++ showSummaries primSummaries

-- | Given a Hydra graph, create a property graph in which the vertices are all elements of the graph
--   (plus all primitives, if selected) and the edges are all direct dependencies between elements
--   (and between elements and primitives, if selected).
--   Each vertex has an id based on the name of the element or primitive, in addition to a "namespace" property.
termGraphToDependencyPropertyGraph :: Bool -> Graph -> PG.Graph String
termGraphToDependencyPropertyGraph withPrimitives g = PG.Graph vertexMap edgeMap
  where
    primitives = graphPrimitives g
    elements = graphElements g
    primLabel = PG.VertexLabel "Primitive"
    elLabel = PG.VertexLabel "Element"
    dependsOnPrimitiveLabel = PG.EdgeLabel "dependsOnPrimitive"
    dependsOnElementLabel = PG.EdgeLabel "dependsOnElement"
    namespaceKey = PG.PropertyKey "namespace"
    nameToId = unName
    nameToNamespace name = Y.maybe "default" unNamespace $ namespaceOf name
    toEdge label outName inName = PG.Edge label edgeId outId inId M.empty
      where
        edgeId = outId ++ "_" ++ PG.unEdgeLabel label ++ "_" ++ inId
        outId = nameToId outName
        inId = nameToId inName
    vertexMap = M.fromList $ fmap (\v -> (PG.vertexId v, v)) vertices
      where
        vertices = prims ++ els
          where
            prims = if withPrimitives
                then fmap toVertex $ M.elems primitives
                else []
              where
                toVertex prim = PG.Vertex primLabel (nameToId $ primitiveName prim) $
                  M.singleton namespaceKey (nameToNamespace $ primitiveName prim)
            els = fmap toVertex $ M.elems elements
              where
                toVertex el = PG.Vertex elLabel (nameToId $ elementName el) $
                  M.singleton namespaceKey (nameToNamespace $ elementName el)
    edgeMap = M.fromList $ fmap (\e -> (PG.edgeId e, e)) edges
      where
        edges = primEdges ++ elEdges
          where
            primEdges = if withPrimitives
                then L.concat $ fmap edgesFrom elements
                else []
              where
                edgesFrom el = fmap (toEdge dependsOnPrimitiveLabel (elementName el)) $
                  S.toList $ termDependencyNames False True False $ elementTerm el
            elEdges = L.concat $ fmap edgesFrom elements
              where
                edgesFrom el = fmap (toEdge dependsOnElementLabel (elementName el)) $
                  S.toList $ freeVariablesInTerm $ elementTerm el

primitiveSummary :: Bool -> Primitive -> String
primitiveSummary withType prim = unName (primitiveName prim)
  ++ if withType
     then (" : " ++ ShowCore.typeScheme (primitiveType prim))
     else ""

primitivesInLibraries :: [Library] -> [Primitive]
primitivesInLibraries libs = L.sortBy (O.comparing primitiveName) $ L.concat $ fmap libraryPrimitives libs

primitivesInStandardLibraries :: [Primitive]
primitivesInStandardLibraries = primitivesInLibraries standardLibraries

showSummaries sums = L.unlines $ fmap (\s -> "  " ++ s) $ L.concat $ fmap L.lines sums
