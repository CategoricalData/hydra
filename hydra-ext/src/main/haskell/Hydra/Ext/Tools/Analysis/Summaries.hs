-- | A module for creating textual summaries of Hydra graphs and programs

{-
:m
import Hydra.Kernel
import Hydra.Ext.Tools.Analysis.Summaries
import Hydra.Sources.Kernel.Types.Core
import Hydra.Generation
import Hydra.Sources.All

-- Create an exhaustive list of Hydra kernel elements and primitives for code generation
either (fail . show) putStrLn $ graphSummary True (modulesToGraph kernelModules) emptyContext
-}
module Hydra.Ext.Tools.Analysis.Summaries where

import Hydra.Kernel
import Hydra.Generation
import Hydra.Ext.Sources.All
import Hydra.Sources.Libraries
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Util as Util
import qualified Hydra.Pg.Model as PG

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Result a = Either Error a

err :: Context -> String -> Result a
err cx msg = Left (ErrorOther (OtherError msg))

elementSummary :: Bool -> Binding -> Graph -> Result String
elementSummary withTypes el g = do
    mt <- findTypeInfo
    return $ (unName $ bindingName el) ++ Y.fromMaybe "" mt
  where
    findTypeInfo = if withTypes
      then case bindingType el of
        Nothing -> Right Nothing
        Just ts -> Just <$> if Predicates.isType (deannotateType $ typeSchemeType ts)
          then case DecodeCore.type_ g $ bindingTerm el of
            Left de -> Left (ErrorDecoding de)
            Right typ -> Right $ " = " ++ ShowCore.type_ (deannotateType typ)
          else Right $ " : " ++ ShowCore.typeScheme ts
      else Right Nothing

elementsInModules :: [Module] -> [Binding]
elementsInModules mods = L.sortBy (O.comparing bindingName) $ L.concat $ fmap moduleBindings mods

elementsInKernelModules :: [Binding]
elementsInKernelModules = elementsInModules kernelModules

fieldSummary :: Field -> String
fieldSummary = unName . fieldName

fieldTypeSummary :: FieldType -> String
fieldTypeSummary (FieldType fname ftype) = unName fname ++ " : " ++ ShowCore.type_ ftype

graphSummary :: Bool -> Graph -> Context -> Result String
graphSummary withTypes g cx = do
  gi <- if withTypes then do
      (result, _cx') <- inferGraphTypes cx (graphToBindings g) g
      pure $ fst result
    else pure g
  let els = L.sortBy (O.comparing bindingName) $ graphToBindings gi
  let prims = L.sortBy (O.comparing primitiveName) $ M.elems $ graphPrimitives gi
  elSummaries <- CM.mapM (\el -> elementSummary withTypes el gi) els
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
    elements = graphToBindings g
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
            els = fmap toVertex elements
              where
                toVertex el = PG.Vertex elLabel (nameToId $ bindingName el) $
                  M.singleton namespaceKey (nameToNamespace $ bindingName el)
    edgeMap = M.fromList $ fmap (\e -> (PG.edgeId e, e)) edges
      where
        edges = primEdges ++ elEdges
          where
            primEdges = if withPrimitives
                then L.concat $ fmap edgesFrom elements
                else []
              where
                edgesFrom el = fmap (toEdge dependsOnPrimitiveLabel (bindingName el)) $
                  S.toList $ termDependencyNames False True False $ bindingTerm el
            elEdges = L.concat $ fmap edgesFrom elements
              where
                edgesFrom el = fmap (toEdge dependsOnElementLabel (bindingName el)) $
                  S.toList $ freeVariablesInTerm $ bindingTerm el

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
