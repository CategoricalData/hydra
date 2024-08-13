-- | Utilities for validating property graphs against property graph schemas

module Hydra.Langs.Tinkerpop.Validate where

import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PropertyGraph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

validateEdge :: ((t -> v -> Maybe String) -> (v -> String) -> Maybe (v -> Maybe PropertyGraph.VertexLabel) -> PropertyGraph.EdgeType t -> PropertyGraph.Edge v -> Maybe String)
validateEdge checkValue showValue labelForVertexId typ el =  
  let failWith = (edgeError showValue el) 
      checkLabel =  
              let expected = (PropertyGraph.edgeTypeLabel typ) 
                  actual = (PropertyGraph.edgeLabel el)
              in (verify (Equality.equalString (PropertyGraph.unEdgeLabel actual) (PropertyGraph.unEdgeLabel expected)) (failWith (prepend "Wrong label" (edgeLabelMismatch expected actual))))
      checkId = (Optionals.map (\x -> failWith (prepend "Invalid id" x)) (checkValue (PropertyGraph.edgeTypeId typ) (PropertyGraph.edgeId el)))
      checkProperties = (Optionals.map (\x -> failWith (prepend "Invalid property" x)) (validateProperties checkValue (PropertyGraph.edgeTypeProperties typ) (PropertyGraph.edgeProperties el)))
      checkOut = ((\x -> case x of
              Nothing -> Nothing
              Just v329 -> ((\x -> case x of
                Nothing -> (Just (failWith (prepend "Out-vertex does not exist" (showValue (PropertyGraph.edgeOut el)))))
                Just v330 -> (verify (Equality.equalString (PropertyGraph.unVertexLabel v330) (PropertyGraph.unVertexLabel (PropertyGraph.edgeTypeOut typ))) (failWith (prepend "Wrong out-vertex label" (vertexLabelMismatch (PropertyGraph.edgeTypeOut typ) v330))))) (v329 (PropertyGraph.edgeOut el)))) labelForVertexId)
      checkIn = ((\x -> case x of
              Nothing -> Nothing
              Just v331 -> ((\x -> case x of
                Nothing -> (Just (failWith (prepend "In-vertex does not exist" (showValue (PropertyGraph.edgeIn el)))))
                Just v332 -> (verify (Equality.equalString (PropertyGraph.unVertexLabel v332) (PropertyGraph.unVertexLabel (PropertyGraph.edgeTypeIn typ))) (failWith (prepend "Wrong in-vertex label" (vertexLabelMismatch (PropertyGraph.edgeTypeIn typ) v332))))) (v331 (PropertyGraph.edgeIn el)))) labelForVertexId)
  in (checkAll [
    checkLabel,
    checkId,
    checkProperties,
    checkOut,
    checkIn])

validateElement :: ((t -> v -> Maybe String) -> (v -> String) -> Maybe (v -> Maybe PropertyGraph.VertexLabel) -> PropertyGraph.ElementType t -> PropertyGraph.Element v -> Maybe String)
validateElement checkValue showValue labelForVertexId typ el = ((\x -> case x of
  PropertyGraph.ElementTypeVertex v333 -> ((\x -> case x of
    PropertyGraph.ElementEdge v334 -> (Just (prepend "Edge instead of vertex" (showValue (PropertyGraph.edgeId v334))))
    PropertyGraph.ElementVertex v335 -> (validateVertex checkValue showValue v333 v335)) el)
  PropertyGraph.ElementTypeEdge v336 -> ((\x -> case x of
    PropertyGraph.ElementVertex v337 -> (Just (prepend "Vertex instead of edge" (showValue (PropertyGraph.vertexId v337))))
    PropertyGraph.ElementEdge v338 -> (validateEdge checkValue showValue labelForVertexId v336 v338)) el)) typ)

validateGraph :: (Ord v) => ((t -> v -> Maybe String) -> (v -> String) -> PropertyGraph.GraphSchema t -> PropertyGraph.Graph v -> Maybe String)
validateGraph checkValue showValue schema graph =  
  let checkVertices =  
          let checkVertex = (\el -> (\x -> case x of
                  Nothing -> (Just (vertexError showValue el (prepend "Unexpected label" (PropertyGraph.unVertexLabel (PropertyGraph.vertexLabel el)))))
                  Just v339 -> (validateVertex checkValue showValue v339 el)) (Maps.lookup (PropertyGraph.vertexLabel el) (PropertyGraph.graphSchemaVertices schema)))
          in (checkAll (Lists.map checkVertex (Maps.values (PropertyGraph.graphVertices graph)))) 
      checkEdges =  
              let checkEdge = (\el -> (\x -> case x of
                      Nothing -> (Just (edgeError showValue el (prepend "Unexpected label" (PropertyGraph.unEdgeLabel (PropertyGraph.edgeLabel el)))))
                      Just v340 -> (validateEdge checkValue showValue labelForVertexId v340 el)) (Maps.lookup (PropertyGraph.edgeLabel el) (PropertyGraph.graphSchemaEdges schema))) 
                  labelForVertexId = (Just (\i -> Optionals.map PropertyGraph.vertexLabel (Maps.lookup i (PropertyGraph.graphVertices graph))))
              in (checkAll (Lists.map checkEdge (Maps.values (PropertyGraph.graphEdges graph))))
  in (checkAll [
    checkVertices,
    checkEdges])

validateProperties :: ((t -> v -> Maybe String) -> [PropertyGraph.PropertyType t] -> Map PropertyGraph.PropertyKey v -> Maybe String)
validateProperties checkValue types props =  
  let checkTypes = (checkAll (Lists.map checkType types)) 
      checkType = (\t -> Logic.ifElse ((\x -> case x of
              Nothing -> (Just (prepend "Missing value for " (PropertyGraph.unPropertyKey (PropertyGraph.propertyTypeKey t))))
              Just _ -> Nothing) (Maps.lookup (PropertyGraph.propertyTypeKey t) props)) Nothing (PropertyGraph.propertyTypeRequired t))
      checkValues =  
              let m = (Maps.fromList (Lists.map (\p -> (PropertyGraph.propertyTypeKey p, (PropertyGraph.propertyTypeValue p))) types)) 
                  checkPair = (\pair ->  
                          let key = (fst pair) 
                              val = (snd pair)
                          in ((\x -> case x of
                            Nothing -> (Just (prepend "Unexpected key" (PropertyGraph.unPropertyKey key)))
                            Just v342 -> (Optionals.map (prepend "Invalid value") (checkValue v342 val))) (Maps.lookup key m)))
              in (checkAll (Lists.map checkPair (Maps.toList props)))
  in (checkAll [
    checkTypes,
    checkValues])

validateVertex :: ((t -> v -> Maybe String) -> (v -> String) -> PropertyGraph.VertexType t -> PropertyGraph.Vertex v -> Maybe String)
validateVertex checkValue showValue typ el =  
  let failWith = (vertexError showValue el) 
      checkLabel =  
              let expected = (PropertyGraph.vertexTypeLabel typ) 
                  actual = (PropertyGraph.vertexLabel el)
              in (verify (Equality.equalString (PropertyGraph.unVertexLabel actual) (PropertyGraph.unVertexLabel expected)) (failWith (prepend "Wrong label" (vertexLabelMismatch expected actual))))
      checkId = (Optionals.map (\x -> failWith (prepend "Invalid id" x)) (checkValue (PropertyGraph.vertexTypeId typ) (PropertyGraph.vertexId el)))
      checkProperties = (Optionals.map (\x -> failWith (prepend "Invalid property" x)) (validateProperties checkValue (PropertyGraph.vertexTypeProperties typ) (PropertyGraph.vertexProperties el)))
  in (checkAll [
    checkLabel,
    checkId,
    checkProperties])

checkAll :: ([Maybe a] -> Maybe a)
checkAll checks =  
  let errors = (Optionals.cat checks)
  in (Lists.safeHead errors)

edgeError :: ((v -> String) -> PropertyGraph.Edge v -> String -> String)
edgeError showValue e = (prepend (Strings.cat [
  "Invalid edge with id ",
  (showValue (PropertyGraph.edgeId e))]))

edgeLabelMismatch :: (PropertyGraph.EdgeLabel -> PropertyGraph.EdgeLabel -> String)
edgeLabelMismatch expected actual = (Strings.cat [
  Strings.cat [
    Strings.cat [
      "expected ",
      (PropertyGraph.unEdgeLabel expected)],
    ", found "],
  (PropertyGraph.unEdgeLabel actual)])

prepend :: (String -> String -> String)
prepend prefix msg = (Strings.cat [
  Strings.cat [
    prefix,
    ": "],
  msg])

verify :: (Bool -> String -> Maybe String)
verify b err = (Logic.ifElse Nothing (Just err) b)

vertexError :: ((v -> String) -> PropertyGraph.Vertex v -> String -> String)
vertexError showValue v = (prepend (Strings.cat [
  "Invalid vertex with id ",
  (showValue (PropertyGraph.vertexId v))]))

vertexLabelMismatch :: (PropertyGraph.VertexLabel -> PropertyGraph.VertexLabel -> String)
vertexLabelMismatch expected actual = (Strings.cat [
  Strings.cat [
    Strings.cat [
      "expected ",
      (PropertyGraph.unVertexLabel expected)],
    ", found "],
  (PropertyGraph.unVertexLabel actual)])