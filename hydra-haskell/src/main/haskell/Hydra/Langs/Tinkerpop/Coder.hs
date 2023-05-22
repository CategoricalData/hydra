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


type ElementAdapter s a t v e p = Adapter s s (Type a) (PG.ElementType t) (Term a) (PG.Element v e p)

type PropertyAdapter s a t p = Adapter s s (FieldType a) (PG.PropertyType t) (Field a) (PG.Property p)

type EdgeIdAdapter s a e = (FieldName, Adapter s s (Type a) () (Term a) e)

type VertexIdAdapter s a v = (FieldName, Adapter s s (Type a) () (Term a) v)

data ProjectionSpec a = ProjectionSpec {
  projectionSpecField :: FieldType a,
  projectionSpecValues :: ValueSpec,
  projectionSpecAlias :: Maybe String}

check :: Bool -> Flow s () -> Flow s ()
check b err = if b then pure () else err

checkRecordName expected actual = check (actual == expected) $
  unexpected ("record of type " ++ unName expected) ("record of type " ++ unName actual)

edgeCoder :: Show a
  => Schema s a t v e p -> Type a -> Name
  -> PG.EdgeLabel -> PG.VertexLabel -> PG.VertexLabel
  -> EdgeIdAdapter s a e -> VertexIdAdapter s a v -> VertexIdAdapter s a v -> [PropertyAdapter s a t p]
  -> ElementAdapter s a t v e p
edgeCoder schema typ tname label outLabel inLabel idAdapter outAdapter inAdapter propAdapters
    = Adapter lossy typ (PG.ElementTypeEdge et) coder
  where
    et = PG.EdgeType label outLabel inLabel $ propertyTypes propAdapters
    coder = Coder encode decode
      where
        encode term = do
          case stripTerm term of
            TermRecord (Record tname' fields) -> do
                checkRecordName tname tname'
                let fieldsm = fieldMap fields
                id <- projectionId fieldsm idAdapter
                props <- encodeProperties fieldsm propAdapters
                outId <- projectionId fieldsm outAdapter
                inId <- projectionId fieldsm inAdapter
                return $ PG.ElementEdge $ PG.Edge label id outId inId props
            _ -> unexpected "record" term
        decode el = noDecoding "edge"

elementCoder :: Schema s Kv t v e p -> Type Kv -> Flow s (ElementAdapter s Kv t v e p)
elementCoder schema typ = case stripType typ of
    TypeRecord (RowType name _ fields) -> withTrace ("adapter for " ++ unName name) $ do
      mOutSpec <- findProjectionSpec name outVertexKey outVertexLabelKey fields
      mInSpec <- findProjectionSpec name inVertexKey inVertexLabelKey fields

      -- TODO: deprecate "kind"
      kind <- case getTypeAnnotation "kind" typ of
        Nothing -> if Y.isNothing mOutSpec || Y.isNothing mInSpec
          then pure PG.ElementKindVertex
          else pure PG.ElementKindEdge
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
          return $ vertexCoder schema typ name label idAdapter propAdapters
        PG.ElementKindEdge -> do
          label <- PG.EdgeLabel <$> findLabelString name edgeLabelKey
          idSpec <- findId name edgeIdKey fields
          idAdapter <- projectionAdapter (schemaEdgeIds schema) idSpec "id"
          let inSpec = Y.fromJust mInSpec
          let outSpec = Y.fromJust mOutSpec
          outAdapter <- projectionAdapter (schemaVertexIds schema) outSpec "out"
          inAdapter <- projectionAdapter (schemaVertexIds schema) inSpec "in"
          outLabel <- Y.maybe (fail "no out-vertex label") (pure . PG.VertexLabel) $ projectionSpecAlias outSpec
          inLabel <- Y.maybe (fail "no in-vertex label") (pure . PG.VertexLabel) $ projectionSpecAlias inSpec
          return $ edgeCoder schema typ name label outLabel inLabel idAdapter outAdapter inAdapter propAdapters

    _ -> unexpected "record type" typ
  where
    vertexIdKey = annotationSchemaVertexId $ schemaAnnotations schema
    edgeIdKey = annotationSchemaEdgeId $ schemaAnnotations schema
    outVertexKey = annotationSchemaOutVertex $ schemaAnnotations schema
    inVertexKey = annotationSchemaInVertex $ schemaAnnotations schema
    outVertexLabelKey = annotationSchemaOutVertexLabel $ schemaAnnotations schema
    inVertexLabelKey = annotationSchemaInVertexLabel $ schemaAnnotations schema
    vertexLabelKey = annotationSchemaVertexLabel $ schemaAnnotations schema
    edgeLabelKey = annotationSchemaEdgeLabel $ schemaAnnotations schema
    propertyKeyKey = annotationSchemaKey $ schemaAnnotations schema
    propertyValueKey = annotationSchemaValue $ schemaAnnotations schema
    ignoreKey = annotationSchemaIgnore $ schemaAnnotations schema

    findLabelString tname labelKey = case getTypeAnnotation labelKey typ of
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

    findProjectionSpec tname key labelKey fields = withTrace ("find " ++ show key ++ " projection") $ do
      mfield <- findField tname key fields
      case mfield of
        Nothing -> pure Nothing
        Just field -> do
          spec <- decodeValueSpec $ Y.fromJust $ getTypeAnnotation key $ fieldTypeType field
          alias <- case getTypeAnnotation labelKey $ fieldTypeType field of
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
              PG.ElementKindVertex -> [vertexIdKey]
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

encodeProperties :: M.Map FieldName (Term a) -> [PropertyAdapter s a t p] -> Flow s (M.Map PG.PropertyKey p)
encodeProperties fields adapters = do
  props <- CM.mapM (encodeProperty fields) adapters
  return $ M.fromList $ fmap (\(PG.Property key val) -> (key, val)) props

encodeProperty :: M.Map FieldName (Term a) -> PropertyAdapter s a t p -> Flow s (PG.Property p)
encodeProperty fields adapter = do
  case M.lookup fname fields of
    Nothing -> fail $ "field not found in record: " ++ unFieldName fname
    Just value -> coderEncode (adapterCoder adapter) (Field fname value)
  where
    fname = fieldTypeName $ adapterSource adapter

-- TODO; infer lossiness
lossy = False

noDecoding :: String -> Flow s x
noDecoding cat = fail $ cat ++ " decoding is not yet supported"

projectionId fields (fname, ad) = case M.lookup fname fields of
  Nothing -> fail $ "no " ++ unFieldName fname ++ " in record"
  Just t -> coderEncode (adapterCoder ad) t

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

traverseToSingleTerm :: String -> (Term a -> Flow s [Term a]) -> Term a -> Flow s (Term a)
traverseToSingleTerm desc traversal term = do
  terms <- traversal term
  case terms of
    [] -> fail $ desc ++ " did not resolve to a term"
    [t] -> pure t
    _ -> fail $ desc ++ " resolved to multiple terms"

vertexCoder :: Show a
  => Schema s a t v e p -> Type a -> Name
  -> PG.VertexLabel -> VertexIdAdapter s a v -> [PropertyAdapter s a t p]
  -> ElementAdapter s a t v e p
vertexCoder schema typ tname label idAdapter propAdapters = Adapter lossy typ (PG.ElementTypeVertex vt) coder
  where
    vt = PG.VertexType label $ propertyTypes propAdapters
    coder = Coder encode decode
      where
        encode term = do
          case stripTerm term of
            TermRecord (Record tname' fields) -> do
                checkRecordName tname tname'
                let fieldsm = fieldMap fields
                id <- projectionId fieldsm idAdapter
                props <- encodeProperties (fieldMap fields) propAdapters
                return $ PG.ElementVertex $ PG.Vertex label id props
            _ -> unexpected "record" term
        decode el = noDecoding "vertex"
