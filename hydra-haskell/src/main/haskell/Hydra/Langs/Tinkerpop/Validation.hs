module Hydra.Langs.Tinkerpop.Validation (
  validateEdge,
  validateEdgeFlow,
  validateElement,
  validateElementFlow,
  validateGraph,
  validateGraphFlow,
  validateProperties,
  validatePropertiesFlow,
  validateVertex,
  validateVertexFlow,
) where

import Hydra.Kernel hiding (Graph(..), Element(..), Edge(..))
import Hydra.Langs.Tinkerpop.PropertyGraph

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Control.Monad as CM


validateEdge :: Ord v => (t -> v -> Maybe String)
               -> (v -> String)
               -> Y.Maybe (v -> Y.Maybe (Vertex v))
               -> EdgeType t
               -> Edge v
               -> Y.Maybe String
validateEdge checkValue showValue lookupVertex typ el = checkAll [checkLabel, checkId, checkProperties, checkOut, checkIn]
  where
    failWith = edgeError showValue el
    checkLabel = verify (actual == expected)
        (failWith $ prepend "Wrong label" $ edgeLabelMismatch expected actual)
      where
        expected = edgeTypeLabel typ
        actual = edgeLabel el
    checkId = fmap (failWith . prepend "Invalid id") $
      checkValue (edgeTypeId typ) (edgeId el)
    checkProperties = fmap (failWith . prepend "Invalid property") $
      validateProperties checkValue (edgeTypeProperties typ) (edgeProperties el)
    checkOut = case lookupVertex of
      Nothing -> Nothing
      Just f -> case f (edgeOut el) of
        Nothing -> Just $ failWith $ prepend "Out-vertex does not exist" $ showValue $ edgeOut el
        Just v -> verify (vertexLabel v == edgeTypeOut typ) $
          failWith $ prepend "Wrong out-vertex label" $ vertexLabelMismatch (edgeTypeOut typ) (vertexLabel v)
    checkIn = case lookupVertex of
      Nothing -> Nothing
      Just f -> case f (edgeIn el) of
        Nothing -> Just $ failWith $ prepend "In-vertex does not exist" $ showValue $ edgeIn el
        Just v -> verify (vertexLabel v == edgeTypeIn typ) $
          failWith $ prepend "Wrong in-vertex label" $ vertexLabelMismatch (edgeTypeIn typ) (vertexLabel v)

validateEdgeFlow :: Ord v => (t -> v -> Maybe String)
               -> (v -> String)
               -> Y.Maybe (v -> Y.Maybe (Vertex v))
               -> EdgeType t
               -> Edge v
               -> Flow s (Edge v)
validateEdgeFlow checkValue showValue lookupVertex typ el
  = asFlow (validateEdge checkValue showValue lookupVertex typ) el

validateElement :: Ord v => (t -> v -> Maybe String)
               -> (v -> String)
               -> Y.Maybe (v -> Y.Maybe (Vertex v))
               -> ElementType t
               -> Element v
               -> Y.Maybe String
validateElement checkValue showValue lookupVertex typ el = case typ of
  ElementTypeVertex vt -> case el of
     ElementEdge e -> Just $ prepend "Edge instead of vertex" $ showValue (edgeId e)
     ElementVertex v -> validateVertex checkValue showValue vt v
  ElementTypeEdge et -> case el of
     ElementVertex v -> Just $ prepend "Vertex instead of edge" $ showValue (vertexId v)
     ElementEdge e -> validateEdge checkValue showValue lookupVertex et e

validateElementFlow :: Ord v => (t -> v -> Maybe String)
               -> (v -> String)
               -> Y.Maybe (v -> Y.Maybe (Vertex v))
               -> ElementType t
               -> Element v
               -> Flow s (Element v)
validateElementFlow checkValue showValue lookupVertex typ el
  = asFlow (validateElement checkValue showValue lookupVertex typ) el

validateGraph :: Ord v => (t -> v -> Maybe String)
                 -> (v -> String)
                 -> GraphSchema t
                 -> Graph v
                 -> Y.Maybe String
validateGraph checkValue showValue schema graph = checkAll [checkVertices, checkEdges]
  where
    checkVertices = checkAll (checkVertex <$> M.elems (graphVertices graph))
    checkEdges = checkAll (checkEdge <$> M.elems (graphEdges graph))
    checkVertex el = case M.lookup (vertexLabel el) (graphSchemaVertices schema) of
        Nothing -> Just $ vertexError showValue el $ prepend "Unexpected label" $ unVertexLabel $ vertexLabel el
        Just t -> validateVertex checkValue showValue t el
    checkEdge el = case M.lookup (edgeLabel el) (graphSchemaEdges schema) of
        Nothing -> Just $ edgeError showValue el $ prepend "Unexpected label" $ unEdgeLabel $ edgeLabel el
        Just t -> validateEdge checkValue showValue (Just lookupVertex) t el
    lookupVertex i = M.lookup i (graphVertices graph)

validateGraphFlow :: Ord v => (t -> v -> Maybe String)
                 -> (v -> String)
                 -> GraphSchema t
                 -> Graph v
                 -> Flow s (Graph v)
validateGraphFlow checkValue showValue schema graph = asFlow (validateGraph checkValue showValue schema) graph

validateProperties :: (t -> v -> Maybe String)
                   -> [PropertyType t]
                   -> M.Map PropertyKey v
                   -> Y.Maybe String
validateProperties checkValue types props = checkAll [checkTypes, checkValues]
  where
    checkTypes = checkAll (checkType <$> types)
    checkType t = if propertyTypeRequired t
      then case M.lookup (propertyTypeKey t) props of
        Nothing -> Just $ prepend "Missing value for " (unPropertyKey $ propertyTypeKey t)
        Just _ -> Nothing
      else Nothing
    checkValues = checkAll (checkPair <$> M.toList props)
      where
        -- Note: it is rather inefficient to do this for every property map
        m = M.fromList $ (\p -> (propertyTypeKey p, propertyTypeValue p)) <$> types
        checkPair (k, v) = case M.lookup k m of
          Nothing -> Just $ prepend "Unexpected key" (unPropertyKey k)
          Just t -> prepend "Invalid value" <$> checkValue t v

validatePropertiesFlow :: (t -> v -> Maybe String)
                   -> [PropertyType t]
                   -> M.Map PropertyKey v
                   -> Flow s (M.Map PropertyKey v)
validatePropertiesFlow checkValue types props = asFlow (validateProperties checkValue types) props

validateVertex :: (t -> v -> Maybe String)
               -> (v -> String)
               -> VertexType t
               -> Vertex v
               -> Y.Maybe String
validateVertex checkValue showValue typ el = checkAll [checkLabel, checkId, checkProperties]
  where
    failWith = vertexError showValue el
    checkLabel = verify (actual == expected)
        (failWith $ prepend "Wrong label" $ vertexLabelMismatch expected actual)
      where
        expected = vertexTypeLabel typ
        actual = vertexLabel el
    checkId = fmap (failWith . prepend "Invalid id") $
      checkValue (vertexTypeId typ) (vertexId el)
    checkProperties = fmap (failWith . prepend "Invalid property") $
      validateProperties checkValue (vertexTypeProperties typ) (vertexProperties el)

validateVertexFlow :: Ord v => (t -> v -> Maybe String)
               -> (v -> String)
               -> VertexType t
               -> Vertex v
               -> Flow s (Vertex v)
validateVertexFlow checkValue showValue typ el
  = asFlow (validateVertex checkValue showValue typ) el

----

asFlow :: (a -> Y.Maybe String) -> a -> Flow s a
asFlow f x = case f x of
  Nothing -> pure x
  Just err -> fail $ prepend "Validation error" err

checkAll :: [Y.Maybe a] -> Y.Maybe a
checkAll checks = case Y.catMaybes checks of
  [] -> Nothing
  (h:_) -> Just h

edgeError :: (v -> String) -> Edge v -> String -> String
edgeError showValue e = prepend ("Invalid edge with id " ++ showValue (edgeId e))

edgeLabelMismatch :: EdgeLabel -> EdgeLabel -> String
edgeLabelMismatch expected actual = "expected " ++ unEdgeLabel expected ++ ", found " ++ unEdgeLabel actual

prepend :: String -> String -> String
prepend prefix msg = prefix ++ ": " ++ msg

verify :: Bool -> a -> Maybe a
verify b err = if b then Nothing else Just err

vertexError :: (v -> String) -> Vertex v -> String -> String
vertexError showValue v = prepend ("Invalid vertex with id " ++ showValue (vertexId v))

vertexLabelMismatch :: VertexLabel -> VertexLabel -> String
vertexLabelMismatch expected actual = "expected " ++ unVertexLabel expected ++ ", found " ++ unVertexLabel actual
