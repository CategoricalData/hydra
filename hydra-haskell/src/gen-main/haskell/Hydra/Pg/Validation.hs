-- | Utilities for validating property graphs against property graph schemas

module Hydra.Pg.Validation where

import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Model as Model
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

validateEdge :: ((t -> v -> Maybe String) -> (v -> String) -> Maybe (v -> Maybe Model.VertexLabel) -> Model.EdgeType t -> Model.Edge v -> Maybe String)
validateEdge checkValue showValue labelForVertexId typ el =  
  let failWith = (edgeError showValue el) 
      checkLabel =  
              let expected = (Model.edgeTypeLabel typ) 
                  actual = (Model.edgeLabel el)
              in (verify (Equality.equalString (Model.unEdgeLabel actual) (Model.unEdgeLabel expected)) (failWith (prepend "Wrong label" (edgeLabelMismatch expected actual))))
      checkId = (Optionals.map (\x -> failWith (prepend "Invalid id" x)) (checkValue (Model.edgeTypeId typ) (Model.edgeId el)))
      checkProperties = (Optionals.map (\x -> failWith (prepend "Invalid property" x)) (validateProperties checkValue (Model.edgeTypeProperties typ) (Model.edgeProperties el)))
      checkOut = ((\x -> case x of
              Nothing -> Nothing
              Just v329 -> ((\x -> case x of
                Nothing -> (Just (failWith (prepend "Out-vertex does not exist" (showValue (Model.edgeOut el)))))
                Just v330 -> (verify (Equality.equalString (Model.unVertexLabel v330) (Model.unVertexLabel (Model.edgeTypeOut typ))) (failWith (prepend "Wrong out-vertex label" (vertexLabelMismatch (Model.edgeTypeOut typ) v330))))) (v329 (Model.edgeOut el)))) labelForVertexId)
      checkIn = ((\x -> case x of
              Nothing -> Nothing
              Just v331 -> ((\x -> case x of
                Nothing -> (Just (failWith (prepend "In-vertex does not exist" (showValue (Model.edgeIn el)))))
                Just v332 -> (verify (Equality.equalString (Model.unVertexLabel v332) (Model.unVertexLabel (Model.edgeTypeIn typ))) (failWith (prepend "Wrong in-vertex label" (vertexLabelMismatch (Model.edgeTypeIn typ) v332))))) (v331 (Model.edgeIn el)))) labelForVertexId)
  in (checkAll [
    checkLabel,
    checkId,
    checkProperties,
    checkOut,
    checkIn])

validateElement :: ((t -> v -> Maybe String) -> (v -> String) -> Maybe (v -> Maybe Model.VertexLabel) -> Model.ElementType t -> Model.Element v -> Maybe String)
validateElement checkValue showValue labelForVertexId typ el = ((\x -> case x of
  Model.ElementTypeVertex v333 -> ((\x -> case x of
    Model.ElementEdge v334 -> (Just (prepend "Edge instead of vertex" (showValue (Model.edgeId v334))))
    Model.ElementVertex v335 -> (validateVertex checkValue showValue v333 v335)) el)
  Model.ElementTypeEdge v336 -> ((\x -> case x of
    Model.ElementVertex v337 -> (Just (prepend "Vertex instead of edge" (showValue (Model.vertexId v337))))
    Model.ElementEdge v338 -> (validateEdge checkValue showValue labelForVertexId v336 v338)) el)) typ)

validateGraph :: (Ord v) => ((t -> v -> Maybe String) -> (v -> String) -> Model.GraphSchema t -> Model.Graph v -> Maybe String)
validateGraph checkValue showValue schema graph =  
  let checkVertices =  
          let checkVertex = (\el -> (\x -> case x of
                  Nothing -> (Just (vertexError showValue el (prepend "Unexpected label" (Model.unVertexLabel (Model.vertexLabel el)))))
                  Just v339 -> (validateVertex checkValue showValue v339 el)) (Maps.lookup (Model.vertexLabel el) (Model.graphSchemaVertices schema)))
          in (checkAll (Lists.map checkVertex (Maps.values (Model.graphVertices graph)))) 
      checkEdges =  
              let checkEdge = (\el -> (\x -> case x of
                      Nothing -> (Just (edgeError showValue el (prepend "Unexpected label" (Model.unEdgeLabel (Model.edgeLabel el)))))
                      Just v340 -> (validateEdge checkValue showValue labelForVertexId v340 el)) (Maps.lookup (Model.edgeLabel el) (Model.graphSchemaEdges schema))) 
                  labelForVertexId = (Just (\i -> Optionals.map Model.vertexLabel (Maps.lookup i (Model.graphVertices graph))))
              in (checkAll (Lists.map checkEdge (Maps.values (Model.graphEdges graph))))
  in (checkAll [
    checkVertices,
    checkEdges])

validateProperties :: ((t -> v -> Maybe String) -> [Model.PropertyType t] -> Map Model.PropertyKey v -> Maybe String)
validateProperties checkValue types props =  
  let checkTypes = (checkAll (Lists.map checkType types)) 
      checkType = (\t -> Logic.ifElse ((\x -> case x of
              Nothing -> (Just (prepend "Missing value for " (Model.unPropertyKey (Model.propertyTypeKey t))))
              Just _ -> Nothing) (Maps.lookup (Model.propertyTypeKey t) props)) Nothing (Model.propertyTypeRequired t))
      checkValues =  
              let m = (Maps.fromList (Lists.map (\p -> (Model.propertyTypeKey p, (Model.propertyTypeValue p))) types)) 
                  checkPair = (\pair ->  
                          let key = (fst pair) 
                              val = (snd pair)
                          in ((\x -> case x of
                            Nothing -> (Just (prepend "Unexpected key" (Model.unPropertyKey key)))
                            Just v342 -> (Optionals.map (prepend "Invalid value") (checkValue v342 val))) (Maps.lookup key m)))
              in (checkAll (Lists.map checkPair (Maps.toList props)))
  in (checkAll [
    checkTypes,
    checkValues])

validateVertex :: ((t -> v -> Maybe String) -> (v -> String) -> Model.VertexType t -> Model.Vertex v -> Maybe String)
validateVertex checkValue showValue typ el =  
  let failWith = (vertexError showValue el) 
      checkLabel =  
              let expected = (Model.vertexTypeLabel typ) 
                  actual = (Model.vertexLabel el)
              in (verify (Equality.equalString (Model.unVertexLabel actual) (Model.unVertexLabel expected)) (failWith (prepend "Wrong label" (vertexLabelMismatch expected actual))))
      checkId = (Optionals.map (\x -> failWith (prepend "Invalid id" x)) (checkValue (Model.vertexTypeId typ) (Model.vertexId el)))
      checkProperties = (Optionals.map (\x -> failWith (prepend "Invalid property" x)) (validateProperties checkValue (Model.vertexTypeProperties typ) (Model.vertexProperties el)))
  in (checkAll [
    checkLabel,
    checkId,
    checkProperties])

checkAll :: ([Maybe a] -> Maybe a)
checkAll checks =  
  let errors = (Optionals.cat checks)
  in (Lists.safeHead errors)

edgeError :: ((v -> String) -> Model.Edge v -> String -> String)
edgeError showValue e = (prepend (Strings.cat [
  "Invalid edge with id ",
  (showValue (Model.edgeId e))]))

edgeLabelMismatch :: (Model.EdgeLabel -> Model.EdgeLabel -> String)
edgeLabelMismatch expected actual = (Strings.cat [
  Strings.cat [
    Strings.cat [
      "expected ",
      (Model.unEdgeLabel expected)],
    ", found "],
  (Model.unEdgeLabel actual)])

prepend :: (String -> String -> String)
prepend prefix msg = (Strings.cat [
  Strings.cat [
    prefix,
    ": "],
  msg])

verify :: (Bool -> String -> Maybe String)
verify b err = (Logic.ifElse Nothing (Just err) b)

vertexError :: ((v -> String) -> Model.Vertex v -> String -> String)
vertexError showValue v = (prepend (Strings.cat [
  "Invalid vertex with id ",
  (showValue (Model.vertexId v))]))

vertexLabelMismatch :: (Model.VertexLabel -> Model.VertexLabel -> String)
vertexLabelMismatch expected actual = (Strings.cat [
  Strings.cat [
    Strings.cat [
      "expected ",
      (Model.unVertexLabel expected)],
    ", found "],
  (Model.unVertexLabel actual)])