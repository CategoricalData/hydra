module Hydra.Langs.Tinkerpop.Coder (
  elementCoder,
) where

import Hydra.Kernel
import Hydra.Langs.Tinkerpop.Mappings
import Hydra.Langs.Tinkerpop.TermsToElements
import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PG
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


type ElementAdapter s a t v e p = Adapter s s (Type a) (PG.ElementTypeTree t) (Term a) (PG.ElementTree v e p)

type PropertyAdapter s a t p = Adapter s s (FieldType a) (PG.PropertyType t) (Field a) (PG.Property p)

type EdgeIdAdapter s a e = (FieldName, Adapter s s (Type a) () (Term a) e)

type VertexIdAdapter s a v = (FieldName, Adapter s s (Type a) () (Term a) v)

data AdjacentEdgeAdapter s a t v e p = AdjacentEdgeAdapter {
  adjacentEdgeAdapterDirection :: PG.Direction,
  adjacentEdgeAdapterField :: FieldType a,
  adjacentEdgeAdapterLabel :: PG.EdgeLabel,
  adjacentEdgeAdapterAdapter :: ElementAdapter s a t v e p}

data ProjectionSpec a = ProjectionSpec {
  projectionSpecField :: FieldType a,
  projectionSpecValues :: ValueSpec,
  projectionSpecAlias :: Maybe String}

check :: Bool -> Flow s () -> Flow s ()
check b err = if b then pure () else err

checkRecordName expected actual = check (actual == expected) $
  unexpected ("record of type " ++ unName expected) ("record of type " ++ unName actual)

edgeCoder :: Show a
  => PG.Direction -> Schema s a t v e p -> Type a -> Name
  -> PG.EdgeLabel -> PG.VertexLabel -> PG.VertexLabel
  -> EdgeIdAdapter s a e -> Maybe (VertexIdAdapter s a v) -> Maybe (VertexIdAdapter s a v) -> [PropertyAdapter s a t p]
  -> ElementAdapter s a t v e p
edgeCoder dir schema source tname label outLabel inLabel idAdapter outAdapter inAdapter propAdapters
    = Adapter lossy source (elementTypeTreeEdge et []) coder
  where
    et = PG.EdgeType label outLabel inLabel $ propertyTypes propAdapters
    coder = Coder encode decode
      where
        encode term = case stripTerm term of
          TermRecord (Record tname' fields) -> do
            checkRecordName tname tname'
            let fieldsm = fieldMap fields
            id <- selectEdgeId fieldsm idAdapter
            props <- encodeProperties fieldsm propAdapters
            outId <- getVertexId PG.DirectionOut fieldsm outAdapter
            inId <- getVertexId PG.DirectionIn fieldsm inAdapter
            return $ elementTreeEdge (PG.Edge label id outId inId props) []
          _ -> unexpected "record" term
        decode el = noDecoding "edge"
        getVertexId dir1 fieldsm adapter = if dir1 == dir
          then pure $ schemaDefaultVertexId schema
          else case adapter of
            Nothing -> fail $ "no adapter"
            Just ad -> selectVertexId fieldsm ad

elementCoder :: (Show v, Show e, Show p) => Y.Maybe (PG.Direction, PG.VertexLabel)
  -> Schema s Kv t v e p -> Type Kv -> Flow s (ElementAdapter s Kv t v e p)
elementCoder mparent schema source = case stripType source of
    TypeRecord (RowType name _ fields) -> withTrace ("adapter for " ++ unName name) $ do
      mOutSpec <- findProjectionSpec name outVertexKey outVertexLabelKey fields
      mInSpec <- findProjectionSpec name inVertexKey inVertexLabelKey fields

      -- TODO: deprecate "kind"
      kind <- case getTypeAnnotation "kind" source of
        Nothing -> pure $ if hasVertexAdapters mOutSpec mInSpec
          then PG.ElementKindEdge
          else PG.ElementKindVertex
        Just kindTerm -> do
          s <- Expect.string kindTerm
          case s of
            "vertex" -> return PG.ElementKindVertex
            "edge" -> if Y.isNothing mOutSpec || Y.isNothing mInSpec
              then fail $ "Record type marked as an edge type, but missing 'out' and/or 'in' fields: " ++ unName name
              else return PG.ElementKindEdge

      propSpecs <- findPropertySpecs kind fields
      propAdapters <- CM.mapM (propertyAdapter schema) propSpecs

      case kind of
        PG.ElementKindVertex -> do
          label <- PG.VertexLabel <$> findLabelString name vertexLabelKey
          idSpec <- findId name vertexIdKey fields
          idAdapter <- projectionAdapter (schemaVertexIds schema) idSpec "id"
          outEdgeAdapters <- edgeAdapters label PG.DirectionOut fields
          inEdgeAdapters <- edgeAdapters label PG.DirectionIn fields
          return $ vertexCoder schema source name label idAdapter propAdapters (outEdgeAdapters ++ inEdgeAdapters)
        PG.ElementKindEdge -> do
          label <- PG.EdgeLabel <$> findLabelString name edgeLabelKey
          idSpec <- findId name edgeIdKey fields
          idAdapter <- projectionAdapter (schemaEdgeIds schema) idSpec "id"
          outAdapter <- Y.maybe (pure Nothing) (\s -> Just <$> projectionAdapter (schemaVertexIds schema) s "out") mOutSpec
          inAdapter <- Y.maybe (pure Nothing) (\s -> Just <$> projectionAdapter (schemaVertexIds schema) s "in") mInSpec
          outLabel <- case mOutSpec of
            Nothing -> pure parentLabel
            Just spec -> Y.maybe (fail "no out-vertex label") (pure . PG.VertexLabel) $ projectionSpecAlias spec
          inLabel <- case mInSpec of
            Nothing -> pure parentLabel
            Just spec -> Y.maybe (fail "no in-vertex label") (pure . PG.VertexLabel) $ projectionSpecAlias spec
          return $ edgeCoder PG.DirectionBoth schema source name label outLabel inLabel idAdapter outAdapter inAdapter propAdapters

    _ -> unexpected "record type" source
  where
    dir = Y.maybe PG.DirectionBoth fst mparent
    parentLabel = Y.maybe (PG.VertexLabel "NOLABEL") snd mparent

    hasVertexAdapters mOutSpec mInSpec = case dir of
      PG.DirectionOut -> Y.isJust mInSpec
      PG.DirectionIn -> Y.isJust mOutSpec
      PG.DirectionBoth -> Y.isJust mOutSpec && Y.isJust mInSpec

    vertexLabelKey = annotationSchemaVertexLabel $ schemaAnnotations schema
    edgeLabelKey = annotationSchemaEdgeLabel $ schemaAnnotations schema
    vertexIdKey = annotationSchemaVertexId $ schemaAnnotations schema
    edgeIdKey = annotationSchemaEdgeId $ schemaAnnotations schema
    propertyKeyKey = annotationSchemaPropertyKey $ schemaAnnotations schema
    propertyValueKey = annotationSchemaPropertyValue $ schemaAnnotations schema
    outVertexKey = annotationSchemaOutVertex $ schemaAnnotations schema
    outVertexLabelKey = annotationSchemaOutVertexLabel $ schemaAnnotations schema
    inVertexKey = annotationSchemaInVertex $ schemaAnnotations schema
    inVertexLabelKey = annotationSchemaInVertexLabel $ schemaAnnotations schema
    outEdgeLabelKey = annotationSchemaOutEdgeLabel $ schemaAnnotations schema
    inEdgeLabelKey = annotationSchemaInEdgeLabel $ schemaAnnotations schema
    ignoreKey = annotationSchemaIgnore $ schemaAnnotations schema

    findLabelString tname labelKey = case getTypeAnnotation labelKey source of
      Nothing -> pure $ unName tname
      Just labelTerm -> Expect.string labelTerm

    findId tname idKey fields = withTrace "find id field" $ do
      mid <- findField tname idKey fields
      case mid of
        Nothing -> fail $ "no " ++ idKey ++ "field"
        Just mi -> do
          spec <- case getTypeAnnotation idKey (fieldTypeType mi) of
            Nothing -> pure ValueSpecValue
            Just t -> decodeValueSpec t
          return $ ProjectionSpec mi spec Nothing

    findProjectionSpec tname key aliasKey fields = withTrace ("find " ++ show key ++ " projection") $ do
      mfield <- findField tname key fields
      case mfield of
        Nothing -> pure Nothing
        Just field -> do
          spec <- decodeValueSpec $ Y.fromJust $ getTypeAnnotation key $ fieldTypeType field
          alias <- case getTypeAnnotation aliasKey $ fieldTypeType field of
            Nothing -> pure Nothing
            Just t -> Just <$> Expect.string t
          return $ Just $ ProjectionSpec field spec alias

    findField tname key fields = withTrace ("find " ++ show key ++ " field") $ do
      let matches = L.filter (\f -> Y.isJust $ getTypeAnnotation key $ fieldTypeType f) fields
      if L.length matches > 1
        then fail $ "Multiple fields marked as '" ++ key ++ "' in record type " ++ unName tname ++ ": "
          ++ (L.intercalate ", " (unFieldName . fieldTypeName <$> matches))
        else return $ if L.null matches then Nothing else Just $ L.head matches

    findPropertySpecs kind fields = CM.mapM toSpec $ L.filter isPropField fields
      where
        isPropField field = not (hasSpecialAnnotation || hasSpecialFieldName)
          where
            hasSpecialAnnotation = L.foldl (\b k -> b || hasAnnotation k) False (ignoreKey:specialKeys)
            hasSpecialFieldName = L.foldl (\b n -> b || hasName n) False specialKeys
            specialKeys = case kind of
              PG.ElementKindVertex -> [vertexIdKey, outEdgeLabelKey, inEdgeLabelKey]
              PG.ElementKindEdge -> [edgeIdKey, outVertexKey, inVertexKey]
            hasAnnotation key = Y.isJust $ getTypeAnnotation key $ fieldTypeType field
            hasName fname = fieldTypeName field == FieldName fname
        toSpec field = do
          alias <- case (getTypeAnnotation propertyKeyKey $ fieldTypeType field) of
            Nothing -> pure Nothing
            Just a -> Just <$> Expect.string a
          values <- case (getTypeAnnotation propertyValueKey $ fieldTypeType field) of
            Nothing -> pure ValueSpecValue
            Just sp -> decodeValueSpec sp
          return $ ProjectionSpec field values alias

    edgeAdapters vlabel dir fields = Y.catMaybes <$> CM.mapM toSpec fields
      where
        toSpec field = case getTypeAnnotation key (fieldTypeType field) of
          Nothing -> pure Nothing
          Just a -> do
            label <- PG.EdgeLabel <$> Expect.string a
            elad <- elementCoder (Just (dir, vlabel)) schema $ fieldTypeType field
            return $ Just $ AdjacentEdgeAdapter dir field label elad
        key = case dir of
          PG.DirectionOut -> outEdgeLabelKey
          PG.DirectionIn -> inEdgeLabelKey

elementTreeEdge :: PG.Edge v e p -> [PG.ElementTree v e p] -> PG.ElementTree v e p
elementTreeEdge e = PG.ElementTree (PG.ElementEdge e)

elementTreeVertex :: PG.Vertex v p -> [PG.ElementTree v e p] -> PG.ElementTree v e p
elementTreeVertex v = PG.ElementTree (PG.ElementVertex v)

elementTypeTreeEdge :: PG.EdgeType t -> [PG.ElementTypeTree t] -> PG.ElementTypeTree t
elementTypeTreeEdge et = PG.ElementTypeTree (PG.ElementTypeEdge et)

elementTypeTreeVertex :: PG.VertexType t -> [PG.ElementTypeTree t] -> PG.ElementTypeTree t
elementTypeTreeVertex vt = PG.ElementTypeTree (PG.ElementTypeVertex vt)

encodeProperties :: Show a => M.Map FieldName (Term a) -> [PropertyAdapter s a t p] -> Flow s (M.Map PG.PropertyKey p)
encodeProperties fields adapters = do
  props <- Y.catMaybes <$> CM.mapM (encodeProperty fields) adapters
  return $ M.fromList $ fmap (\(PG.Property key val) -> (key, val)) props

encodeProperty :: Show a => M.Map FieldName (Term a) -> PropertyAdapter s a t p -> Flow s (Maybe (PG.Property p))
encodeProperty fields adapter = do
  case M.lookup fname fields of
    Nothing -> case ftyp of
      TypeOptional _ -> pure Nothing
      _ -> fail $ "expected field not found in record: " ++ unFieldName fname
    Just value -> case ftyp of
      TypeOptional _ -> case stripTerm value of
        TermOptional ov -> case ov of
          Nothing -> pure Nothing
          Just v -> Just <$> encodeValue v
        _ -> unexpected "optional term" value
      _ -> Just <$> encodeValue value
  where
    fname = fieldTypeName $ adapterSource adapter
    ftyp = stripType (fieldTypeType $ adapterSource adapter)
    encodeValue v = coderEncode (adapterCoder adapter) (Field fname v)

-- TODO; infer lossiness
lossy = True

noDecoding :: String -> Flow s x
noDecoding cat = fail $ cat ++ " decoding is not yet supported"

projectionAdapter :: Show a => Coder s s (Term a) v
  -> ProjectionSpec a
  -> String
  -> Flow s (VertexIdAdapter s a v)
projectionAdapter coder spec key = do
    traversal <- parseValueSpec $ projectionSpecValues spec
    let field = projectionSpecField spec
    let encode = \t -> traverseToSingleTerm (key ++ "-projection") traversal t >>= coderEncode coder
    return (fieldTypeName field, Adapter lossy (fieldTypeType field) () $ Coder encode decode)
  where
    decode _ = noDecoding $ "edge '" ++ key ++ "'"

propertyAdapter :: Show a => Schema s a t v e p -> ProjectionSpec a -> Flow s (PropertyAdapter s a t p)
propertyAdapter schema (ProjectionSpec tfield values alias) = do
  let key = PG.PropertyKey $ case alias of
        Nothing -> unFieldName $ fieldTypeName tfield
        Just k -> k
  pt <- coderEncode (schemaPropertyTypes schema) $ fieldTypeType tfield
  traversal <- parseValueSpec values
  let coder = Coder encode decode
        where
          encode dfield = withTrace ("encode property field " ++ show (unFieldName $ fieldTypeName tfield)) $ do
            if fieldName dfield /= fieldTypeName tfield
              then unexpected ("field '" ++ unFieldName (fieldTypeName tfield) ++ "'") dfield
              else do
                result <- traverseToSingleTerm "property traversal" traversal $ fieldTerm dfield
                value <- coderEncode (schemaPropertyValues schema) result
                return $ PG.Property key value
          decode _ = noDecoding "property"
  return $ Adapter lossy tfield (PG.PropertyType key pt) coder

propertyTypes propAdapters = M.fromList $
  fmap (\a -> (PG.propertyTypeKey $ adapterTarget a, PG.propertyTypeValue $ adapterTarget a)) propAdapters

selectEdgeId fields (fname, ad) = case M.lookup fname fields of
  Nothing -> fail $ "no " ++ unFieldName fname ++ " in record"
  Just t -> coderEncode (adapterCoder ad) t

selectVertexId :: Show a => M.Map FieldName (Term a) -> VertexIdAdapter s a v -> Flow s v
selectVertexId  fields (fname, ad) = case M.lookup fname fields of
  Nothing -> fail $ "no " ++ unFieldName fname ++ " in record"
  Just t -> coderEncode (adapterCoder ad) t

traverseToSingleTerm :: String -> (Term a -> Flow s [Term a]) -> Term a -> Flow s (Term a)
traverseToSingleTerm desc traversal term = do
  terms <- traversal term
  case terms of
    [] -> fail $ desc ++ " did not resolve to a term"
    [t] -> pure t
    _ -> fail $ desc ++ " resolved to multiple terms"

vertexCoder :: (Show a, Show v, Show e, Show p)
  => Schema s a t v e p -> Type a -> Name
  -> PG.VertexLabel -> VertexIdAdapter s a v -> [PropertyAdapter s a t p]
  -> [AdjacentEdgeAdapter s a t v e p]
  -> ElementAdapter s a t v e p
vertexCoder schema source tname label idAdapter propAdapters edgeAdapters = Adapter lossy source target coder
  where
    target = elementTypeTreeVertex vt depTypes
    vt = PG.VertexType label $ propertyTypes propAdapters
    depTypes = adapterTarget . adjacentEdgeAdapterAdapter <$> edgeAdapters
    coder = Coder encode decode
      where
        encode term = case stripTerm term of
            TermRecord (Record tname' fields) -> do
              checkRecordName tname tname'
              let fieldsm = fieldMap fields
              vid <- selectVertexId fieldsm idAdapter
              props <- encodeProperties (fieldMap fields) propAdapters
              deps <- Y.catMaybes <$> CM.mapM (findDeps vid fieldsm) edgeAdapters
              return $ elementTreeVertex (PG.Vertex label vid props) deps
            _ -> unexpected "record" term
          where
            findDeps vid fieldsm (AdjacentEdgeAdapter dir field label ad) = do
                case M.lookup (fieldTypeName field) fieldsm of
                  Nothing -> pure Nothing
                  Just fterm -> Just <$> (coderEncode (adapterCoder ad) fterm >>= fixTree)
              where
                fixTree tree = case PG.elementTreePrimary tree of
                  PG.ElementEdge e -> pure $ tree {PG.elementTreePrimary = PG.ElementEdge $ fixEdge e}
                  _ -> unexpected "edge tree" tree
                fixEdge e = case dir of
                  PG.DirectionOut -> e {PG.edgeOut = vid}
                  PG.DirectionIn -> e {PG.edgeIn = vid}
        decode el = noDecoding "vertex"
