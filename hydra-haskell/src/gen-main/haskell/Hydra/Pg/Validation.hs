-- | Utilities for validating property graphs against property graph schemas

module Hydra.Pg.Validation where

import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Model as Model
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

validateEdge :: ((t0 -> t1 -> Maybe String) -> (t1 -> String) -> Maybe (t1 -> Maybe Model.VertexLabel) -> Model.EdgeType t0 -> Model.Edge t1 -> Maybe String)
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
              Just v1 -> ((\x -> case x of
                Nothing -> (Just (failWith (prepend "Out-vertex does not exist" (showValue (Model.edgeOut el)))))
                Just v2 -> (verify (Equality.equalString (Model.unVertexLabel v2) (Model.unVertexLabel (Model.edgeTypeOut typ))) (failWith (prepend "Wrong out-vertex label" (vertexLabelMismatch (Model.edgeTypeOut typ) v2))))) (v1 (Model.edgeOut el)))) labelForVertexId)
      checkIn = ((\x -> case x of
              Nothing -> Nothing
              Just v1 -> ((\x -> case x of
                Nothing -> (Just (failWith (prepend "In-vertex does not exist" (showValue (Model.edgeIn el)))))
                Just v2 -> (verify (Equality.equalString (Model.unVertexLabel v2) (Model.unVertexLabel (Model.edgeTypeIn typ))) (failWith (prepend "Wrong in-vertex label" (vertexLabelMismatch (Model.edgeTypeIn typ) v2))))) (v1 (Model.edgeIn el)))) labelForVertexId)
  in (checkAll [
    checkLabel,
    checkId,
    checkProperties,
    checkOut,
    checkIn])

validateElement :: ((t0 -> t1 -> Maybe String) -> (t1 -> String) -> Maybe (t1 -> Maybe Model.VertexLabel) -> Model.ElementType t0 -> Model.Element t1 -> Maybe String)
validateElement checkValue showValue labelForVertexId typ el = ((\x -> case x of
  Model.ElementTypeVertex v1 -> ((\x -> case x of
    Model.ElementEdge v2 -> (Just (prepend "Edge instead of vertex" (showValue (Model.edgeId v2))))
    Model.ElementVertex v2 -> (validateVertex checkValue showValue v1 v2)) el)
  Model.ElementTypeEdge v1 -> ((\x -> case x of
    Model.ElementVertex v2 -> (Just (prepend "Vertex instead of edge" (showValue (Model.vertexId v2))))
    Model.ElementEdge v2 -> (validateEdge checkValue showValue labelForVertexId v1 v2)) el)) typ)

validateGraph :: (Ord t1) => ((t0 -> t1 -> Maybe String) -> (t1 -> String) -> Model.GraphSchema t0 -> Model.Graph t1 -> Maybe String)
validateGraph checkValue showValue schema graph =  
  let checkVertices =  
          let checkVertex = (\el -> (\x -> case x of
                  Nothing -> (Just (vertexError showValue el (prepend "Unexpected label" (Model.unVertexLabel (Model.vertexLabel el)))))
                  Just v1 -> (validateVertex checkValue showValue v1 el)) (Maps.lookup (Model.vertexLabel el) (Model.graphSchemaVertices schema)))
          in (checkAll (Lists.map checkVertex (Maps.values (Model.graphVertices graph)))) 
      checkEdges =  
              let checkEdge = (\el -> (\x -> case x of
                      Nothing -> (Just (edgeError showValue el (prepend "Unexpected label" (Model.unEdgeLabel (Model.edgeLabel el)))))
                      Just v1 -> (validateEdge checkValue showValue labelForVertexId v1 el)) (Maps.lookup (Model.edgeLabel el) (Model.graphSchemaEdges schema))) 
                  labelForVertexId = (Just (\i -> Optionals.map Model.vertexLabel (Maps.lookup i (Model.graphVertices graph))))
              in (checkAll (Lists.map checkEdge (Maps.values (Model.graphEdges graph))))
  in (checkAll [
    checkVertices,
    checkEdges])

validateProperties :: ((t1 -> t0 -> Maybe String) -> [Model.PropertyType t1] -> M.Map Model.PropertyKey t0 -> Maybe String)
validateProperties checkValue types props =  
  let checkTypes = (checkAll (Lists.map checkType types)) 
      checkType = (\t -> Logic.ifElse (Model.propertyTypeRequired t) ((\x -> case x of
              Nothing -> (Just (prepend "Missing value for " (Model.unPropertyKey (Model.propertyTypeKey t))))
              Just _ -> Nothing) (Maps.lookup (Model.propertyTypeKey t) props)) Nothing)
      checkValues =  
              let m = (Maps.fromList (Lists.map (\p -> (Model.propertyTypeKey p, (Model.propertyTypeValue p))) types)) 
                  checkPair = (\pair ->  
                          let key = (fst pair) 
                              val = (snd pair)
                          in ((\x -> case x of
                            Nothing -> (Just (prepend "Unexpected key" (Model.unPropertyKey key)))
                            Just v1 -> (Optionals.map (prepend "Invalid value") (checkValue v1 val))) (Maps.lookup key m)))
              in (checkAll (Lists.map checkPair (Maps.toList props)))
  in (checkAll [
    checkTypes,
    checkValues])

validateVertex :: ((t0 -> t1 -> Maybe String) -> (t1 -> String) -> Model.VertexType t0 -> Model.Vertex t1 -> Maybe String)
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

checkAll :: ([Maybe t0] -> Maybe t0)
checkAll checks =  
  let errors = (Optionals.cat checks)
  in (Lists.safeHead errors)

edgeError :: ((t0 -> String) -> Model.Edge t0 -> String -> String)
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

verify :: (Bool -> t0 -> Maybe t0)
verify b err = (Logic.ifElse b Nothing (Just err))

vertexError :: ((t0 -> String) -> Model.Vertex t0 -> String -> String)
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