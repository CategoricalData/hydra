module Hydra.Langs.Tinkerpop.Validation (
  validateGraph,
  validateVertex,
  validateEdge,
  validateProperties
) where

import Hydra.Kernel hiding (Graph(..), Edge(..))
import Hydra.Langs.Tinkerpop.PropertyGraph
import Hydra.Langs.Tinkerpop.Errors

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Control.Monad as CM


validateGraph :: Ord v => (t -> v -> Y.Maybe (TypeError t v))
                 -> GraphSchema t
                 -> Graph v
                 -> Y.Maybe (ElementValidationError t v)
validateGraph checkValue schema graph = checkAll [checkVertices, checkEdges]
  where
    checkVertices = checkAll (checkVertex <$> M.elems (graphVertices graph))
    checkEdges = checkAll (checkEdge <$> M.elems (graphEdges graph))
    checkVertex el = case M.lookup (vertexLabel el) (graphSchemaVertices schema) of
        Nothing -> verror $ BadVertexLabelUnexpected $ vertexLabel el
        Just t -> ElementValidationErrorVertex <$> validateVertex checkValue t el
      where
        verror err = Just $ ElementValidationErrorVertex $ VertexValidationError (vertexId el) err
    checkEdge el = case M.lookup (edgeLabel el) (graphSchemaEdges schema) of
        Nothing -> eerror $ BadEdgeLabelUnexpected $ edgeLabel el
        Just t -> ElementValidationErrorEdge <$> validateEdge checkValue (Just lookupVertex) t el
      where
        eerror err = Just $ ElementValidationErrorEdge $ EdgeValidationError (edgeId el) err
    lookupVertex i = M.lookup i (graphVertices graph)

validateVertex :: (t -> v -> Y.Maybe (TypeError t v))
               -> VertexType t
               -> Vertex v
               -> Y.Maybe (VertexValidationError t v)
validateVertex checkValue typ el = checkAll [checkLabel, checkId, checkProperties]
  where
    verror = VertexValidationError (vertexId el)
    checkLabel = verify (actual == expected)
        (verror $ BadVertexLabel $ VertexLabelMismatch expected actual)
      where
        expected = vertexTypeLabel typ
        actual = vertexLabel el
    checkId = fmap (verror . BadVertexId) $
      checkValue (vertexTypeId typ) (vertexId el)
    checkProperties = fmap (verror . BadVertexProperty) $
      validateProperties checkValue (vertexTypeProperties typ) (vertexProperties el)

validateEdge :: Ord v => (t -> v -> Y.Maybe (TypeError t v))
               -> Y.Maybe (v -> Y.Maybe (Vertex v))
               -> EdgeType t
               -> Edge v
               -> Y.Maybe (EdgeValidationError t v)
validateEdge checkValue lookupVertex typ el = checkAll [checkLabel, checkId, checkProperties, checkOut, checkIn]
  where
    verror = EdgeValidationError (edgeId el)
    checkLabel = verify (actual == expected)
        (verror $ BadEdgeLabel $ EdgeLabelMismatch expected actual)
      where
        expected = edgeTypeLabel typ
        actual = edgeLabel el
    checkId = fmap (verror . BadEdgeId) $
      checkValue (edgeTypeId typ) (edgeId el)
    checkProperties = fmap (verror . BadEdgeProperty) $
      validateProperties checkValue (edgeTypeProperties typ) (edgeProperties el)
    checkOut = case lookupVertex of
      Nothing -> Nothing
      Just f -> case f (edgeOut el) of
        Nothing -> Just $ verror $ BadEdgeNoSuchOutVertex $ edgeOut el
        Just v -> verify (vertexLabel v == edgeTypeOut typ) $
          verror $ BadEdgeWrongOutVertexLabel $ VertexLabelMismatch (edgeTypeOut typ) (vertexLabel v)
    checkIn = case lookupVertex of
      Nothing -> Nothing
      Just f -> case f (edgeIn el) of
        Nothing -> Just $ verror $ BadEdgeNoSuchInVertex $ edgeIn el
        Just v -> verify (vertexLabel v == edgeTypeIn typ) $
          verror $ BadEdgeWrongInVertexLabel $ VertexLabelMismatch (edgeTypeIn typ) (vertexLabel v)

validateProperties :: (t -> v -> Y.Maybe (TypeError t v))
                   -> [PropertyType t]
                   -> M.Map PropertyKey v
                   -> Y.Maybe (BadProperty t v)
validateProperties checkValue types props = checkAll [checkTypes, checkValues]
  where
    checkTypes = checkAll (checkType <$> types)
    checkType t = if propertyTypeRequired t
      then case M.lookup (propertyTypeKey t) props of
        Nothing -> Just $ BadPropertyMissingKey $ propertyTypeKey t
        Just _ -> Nothing
      else Nothing
    checkValues = checkAll (checkPair <$> M.toList props)
      where
        -- Note: it is rather inefficient to do this for every property map
        m = M.fromList $ (\p -> (propertyTypeKey p, propertyTypeValue p)) <$> types
        checkPair (k, v) = case M.lookup k m of
          Nothing -> Just $ BadPropertyUnexpectedKey k
          Just t -> BadPropertyValue <$> checkValue t v

checkAll :: [Y.Maybe a] -> Y.Maybe a
checkAll checks = case Y.catMaybes checks of
  [] -> Nothing
  (h:_) -> Just h

verify :: Bool -> a -> Maybe a
verify b err = if b then Nothing else Just err
