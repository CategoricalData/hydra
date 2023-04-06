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

type PropAdapter s a t p = Adapter s s (FieldType a) (PG.PropertyType t) (Field a) (PG.Property p)

type EdgeIdAdapter s a e = (FieldName, Adapter s s (Type a) () (Term a) e)

type VertexIdAdapter s a v = (FieldName, Adapter s s (Type a) () (Term a) v)

data ProjectionSpec a = ProjectionSpec {
  projectionSpecField :: FieldType a,
  projectionSpecValues :: ValueSpec}

check :: Bool -> Flow s () -> Flow s ()
check b err = if b then pure () else err

checkRecordName expected actual = check (actual == expected) $
  unexpected ("record of type " ++ unName expected) ("record of type " ++ unName actual)

edgeCoder :: Show a
  => Schema s a t v e p -> Type a -> Name -> PG.EdgeLabel
  -> EdgeIdAdapter s a e -> VertexIdAdapter s a v -> VertexIdAdapter s a v -> [PropAdapter s a t p]
  -> ElementAdapter s a t v e p
edgeCoder schema typ tname label idAdapter outAdapter inAdapter propAdapters = Adapter lossy typ (PG.ElementTypeEdge et) coder
  where
    et = PG.EdgeType label outt int $ propertyTypes propAdapters
    outt = PG.VertexLabel "TODO" -- TODO
    int = PG.VertexLabel "TODO" -- TODO
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
      outSpec <- findProjectionSpec name outVertexKey fields
      inSpec <- findProjectionSpec name inVertexKey fields

      kind <- case getTypeAnnotation "kind" typ of
        Nothing -> if Y.isNothing outSpec || Y.isNothing inSpec
          then pure PG.ElementKindVertex
          else pure PG.ElementKindEdge
        Just kindTerm -> do
          s <- Expect.string kindTerm
          case s of
            "vertex" -> return PG.ElementKindVertex
            "edge" -> if Y.isNothing outSpec || Y.isNothing inSpec
              then fail $ "Record type marked as an edge type, but missing 'out' and/or 'in' fields: " ++ unName name
              else return PG.ElementKindEdge

      let propFields = findPropertyFields kind fields
      propAdapters <- CM.mapM (propertyAdapter schema) propFields

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
          outAdapter <- projectionAdapter (schemaVertexIds schema) (Y.fromJust outSpec) "out"
          inAdapter <- projectionAdapter (schemaVertexIds schema) (Y.fromJust inSpec) "in"
          return $ edgeCoder schema typ name label idAdapter outAdapter inAdapter propAdapters

    _ -> unexpected "record type" typ
  where
    vertexIdKey = annotationSchemaVertexId $ schemaAnnotations schema
    edgeIdKey = annotationSchemaEdgeId $ schemaAnnotations schema
    outVertexKey = annotationSchemaOutVertex $ schemaAnnotations schema
    inVertexKey = annotationSchemaInVertex $ schemaAnnotations schema
    vertexLabelKey = annotationSchemaVertexLabel $ schemaAnnotations schema
    edgeLabelKey = annotationSchemaEdgeLabel $ schemaAnnotations schema
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
          return $ ProjectionSpec mi spec

    findProjectionSpec tname key fields = withTrace ("find " ++ show key ++ " projection") $ do
      mfield <- findField tname key fields
      case mfield of
        Nothing -> pure Nothing
        Just field -> do
          spec <- decodeValueSpec $ Y.fromJust $ getTypeAnnotation key (fieldTypeType field)
          return $ Just $ ProjectionSpec field spec

    findField tname key fields = withTrace ("find " ++ show key ++ " field") $ do
      let explicit = L.filter (\f -> Y.isJust $ getTypeAnnotation key $ fieldTypeType f) fields
      if L.length explicit > 1
        then fail $ "Multiple fields marked as '" ++ key ++ "' in record type " ++ unName tname ++ ": "
          ++ (L.intercalate ", " (unFieldName . fieldTypeName <$> explicit))
        else if L.null explicit
        then do
          let implicit = L.filter (\f -> key == unFieldName (fieldTypeName f)) fields
          if L.null implicit
            then return Nothing
            else return $ Just $ L.head implicit
        else return $ Just $ L.head explicit

    findPropertyFields kind fields = L.filter isPropField fields
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

    projectionAdapter coder spec key = do
        fun <- parseValueSpec $ projectionSpecValues spec
        let field = projectionSpecField spec
        return (fieldTypeName field, Adapter lossy (fieldTypeType field) () $ Coder (encode fun) decode)
      where
        encode fun term = do
          terms <- fun term
          case terms of
            [] -> fail $ key ++ "-projection did not resolve to a term"
            [t] -> coderEncode coder t
            _ -> fail $ key ++ "-projection resolved to multiple terms"
        decode _ = noDecoding $ "edge '" ++ key ++ "'"

encodeProperties :: M.Map FieldName (Term a) -> [PropAdapter s a t p] -> Flow s (M.Map PG.PropertyKey p)
encodeProperties fields adapters = do
  props <- CM.mapM (encodeProperty fields) adapters
  return $ M.fromList $ fmap (\(PG.Property key val) -> (key, val)) props

encodeProperty :: M.Map FieldName (Term a) -> PropAdapter s a t p -> Flow s (PG.Property p)
encodeProperty fields adapter = do
  case M.lookup fname fields of
    Nothing -> fail $ "field not found in record: " ++ unFieldName fname
    Just value -> coderEncode (adapterCoder adapter) (Field fname value)
  where
    fname = fieldTypeName $ adapterSource adapter

fieldMap :: [Field a] -> M.Map FieldName (Term a)
fieldMap fields = M.fromList (toPair <$> fields)
  where
    toPair f = (fieldName f, fieldTerm f)

-- TODO; infer lossiness
lossy = False

noDecoding :: String -> Flow s x
noDecoding cat = fail $ cat ++ " decoding is not yet supported"

projectionId fields (fname, ad) = case M.lookup fname fields of
  Nothing -> fail $ "no " ++ unFieldName fname ++ " in record"
  Just t -> coderEncode (adapterCoder ad) t

propertyAdapter :: Show a => Schema s a t v e p -> FieldType a -> Flow s (PropAdapter s a t p)
propertyAdapter schema tfield = do
  let key = PG.PropertyKey $ unFieldName $ fieldTypeName tfield
  pt <- coderEncode (schemaPropertyTypes schema) $ fieldTypeType tfield
  let coder = Coder encode decode
        where
          encode dfield = withTrace ("encode property field " ++ show (unFieldName $ fieldTypeName tfield)) $ do
            if fieldName dfield /= fieldTypeName tfield
              then unexpected ("field '" ++ unFieldName (fieldTypeName tfield) ++ "'") dfield
              else do
                value <- coderEncode (schemaPropertyValues schema) $ fieldTerm dfield
                return $ PG.Property key value
          decode _ = noDecoding "property"
  return $ Adapter lossy tfield (PG.PropertyType key pt) coder

propertyTypes propAdapters = M.fromList $
  fmap (\a -> (PG.propertyTypeKey $ adapterTarget a, PG.propertyTypeValue $ adapterTarget a)) propAdapters

vertexCoder :: Show a
  => Schema s a t v e p -> Type a -> Name
  -> PG.VertexLabel -> VertexIdAdapter s a v -> [PropAdapter s a t p]
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
