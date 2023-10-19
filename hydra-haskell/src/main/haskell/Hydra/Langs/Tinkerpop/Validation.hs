module Hydra.Langs.Tinkerpop.Validation (
  validateGraph,
  validateVertex,
  validateEdge,
  validateProperties
) where

import Hydra.Kernel hiding (Graph(..), Edge(..))
import Hydra.Langs.Tinkerpop.PropertyGraph
import Hydra.Langs.Tinkerpop.Errors

import qualified Data.Map as M
import qualified Control.Monad as CM


validateGraph :: Ord v => (t -> v -> Either (TypeError t v) ())
                 -> GraphSchema t
                 -> Graph v
                 -> Either (ElementValidationError t v) ()
validateGraph checkValue schema graph = checkAll [checkVertices, checkEdges]
  where
    checkVertices = checkAll (checkVertex <$> M.elems (graphVertices graph))
    checkEdges = checkAll (checkEdge <$> M.elems (graphEdges graph))
    checkVertex el = case M.lookup (vertexLabel el) (graphSchemaVertices schema) of
        Nothing -> verror $ BadVertexLabelUnexpected $ vertexLabel el
        Just t -> case validateVertex checkValue t el of
          Left err -> Left $ ElementValidationErrorVertex err
          Right () -> Right ()
      where
        verror err = Left $ ElementValidationErrorVertex $ VertexValidationError (vertexId el) err
    checkEdge el = case M.lookup (edgeLabel el) (graphSchemaEdges schema) of
        Nothing -> eerror $ BadEdgeLabelUnexpected $ edgeLabel el
        Just t -> case validateEdge checkValue (Just lookupVertex) t el of
          Left err -> Left $ ElementValidationErrorEdge err
          Right () -> Right ()
      where
        eerror err = Left $ ElementValidationErrorEdge $ EdgeValidationError (edgeId el) err
    lookupVertex i = M.lookup i (graphVertices graph)

validateVertex :: (t -> v -> Either (TypeError t v) ())
               -> VertexType t
               -> Vertex v
               -> Either (VertexValidationError t v) ()
validateVertex checkValue typ el = checkAll [checkLabel, checkId, checkProperties]
  where
    verror = VertexValidationError (vertexId el)
    checkLabel = verify (actual == expected)
        (verror $ BadVertexLabel $ VertexLabelMismatch expected actual)
      where
        expected = vertexTypeLabel typ
        actual = vertexLabel el
    checkId = mapLeft (verror . BadVertexId) $
      checkValue (vertexTypeId typ) (vertexId el)
    checkProperties = mapLeft (verror . BadVertexProperty) $
      validateProperties checkValue (vertexTypeProperties typ) (vertexProperties el)

validateEdge :: Ord v => (t -> v -> Either (TypeError t v) ())
               -> Maybe (v -> Maybe (Vertex v))
               -> EdgeType t
               -> Edge v
               -> Either (EdgeValidationError t v) ()
validateEdge checkValue lookupVertex typ el = checkAll [checkLabel, checkId, checkProperties, checkOut, checkIn]
  where
    verror = EdgeValidationError (edgeId el)
    checkLabel = verify (actual == expected)
        (verror $ BadEdgeLabel $ EdgeLabelMismatch expected actual)
      where
        expected = edgeTypeLabel typ
        actual = edgeLabel el
    checkId = mapLeft (verror . BadEdgeId) $
      checkValue (edgeTypeId typ) (edgeId el)
    checkProperties = mapLeft (verror . BadEdgeProperty) $
      validateProperties checkValue (edgeTypeProperties typ) (edgeProperties el)
    checkOut = case lookupVertex of
      Nothing -> Right ()
      Just f -> case f (edgeOut el) of
        Nothing -> Left $ verror $ BadEdgeNoSuchOutVertex $ edgeOut el
        Just v -> verify (vertexLabel v == edgeTypeOut typ) $
          verror $ BadEdgeWrongOutVertexLabel $ VertexLabelMismatch (edgeTypeOut typ) (vertexLabel v)
    checkIn = case lookupVertex of
      Nothing -> Right ()
      Just f -> case f (edgeIn el) of
        Nothing -> Left $ verror $ BadEdgeNoSuchInVertex $ edgeIn el
        Just v -> verify (vertexLabel v == edgeTypeIn typ) $
          verror $ BadEdgeWrongInVertexLabel $ VertexLabelMismatch (edgeTypeIn typ) (vertexLabel v)

validateProperties :: (t -> v -> Either (TypeError t v) ())
                   -> [PropertyType t]
                   -> M.Map PropertyKey v
                   -> Either (BadProperty t v) ()
validateProperties checkValue types props = checkAll [checkTypes, checkValues]
  where
    checkTypes = checkAll (checkType <$> types)
    checkType t = if propertyTypeRequired t
      then case M.lookup (propertyTypeKey t) props of
        Nothing -> Left $ BadPropertyMissingKey $ propertyTypeKey t
        Just _ -> Right ()
      else Right ()
    checkValues = checkAll (checkPair <$> M.toList props)
      where
        -- Note: it is rather inefficient to do this for every property map
        m = M.fromList $ (\p -> (propertyTypeKey p, propertyTypeValue p)) <$> types
        checkPair (k, v) = case M.lookup k m of
          Nothing -> Left $ BadPropertyUnexpectedKey k
          Just t -> case checkValue t v of
            Left err -> Left $ BadPropertyValue err
            Right _ -> Right ()

checkAll :: [Either a ()] -> Either a ()
checkAll checks = case CM.sequence checks of
  Left err -> Left err
  Right _ -> Right ()

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f result = case result of
  Left err -> Left $ f err
  Right x -> Right x

verify :: Bool -> a -> Either a ()
verify b err = if b then Right () else Left err
