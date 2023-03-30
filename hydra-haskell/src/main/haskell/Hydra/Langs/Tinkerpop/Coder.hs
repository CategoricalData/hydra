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

data ProjectionType = ProjectionTypeId | ProjectionTypeValue

data ProjectionSpec a = ProjectionSpec {
  projectionSpecType :: ProjectionType,
  projectionSpecField :: FieldType a,
  projectionSpecValues :: ValueSpec}

check :: Bool -> Flow s () -> Flow s ()
check b err = if b then pure () else err

checkRecordName expected actual = check (actual == expected) $
  unexpected ("record of type " ++ unName expected) ("record of type " ++ unName actual)

edgeCoder :: Show a
  => Schema s a t v e p -> Type a -> Name
  -> PG.EdgeLabel -> EdgeIdAdapter s a e -> VertexIdAdapter s a v -> VertexIdAdapter s a v -> [PropAdapter s a t p]
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
    TypeRecord (RowType name _ fields) -> do
      labelStr <- findLabelString name

      idProj <- findId name fields
      outProj <- findProjection name "out" fields
      inProj <- findProjection name "in" fields
      
      let propFields = findPropertyFields fields
      propAdapters <- CM.mapM (propertyAdapter schema) propFields
      
      kind <- case getTypeAnnotation "kind" typ of
        Nothing -> if Y.isNothing outProj || Y.isNothing inProj
          then pure PG.ElementKindVertex
          else pure PG.ElementKindEdge
        Just kindTerm -> do
          s <- Expect.string kindTerm
          case s of
            "vertex" -> return PG.ElementKindVertex
            "edge" -> if Y.isNothing outProj || Y.isNothing inProj
              then fail $ "Record type marked as an edge type, but missing 'out' and/or 'in' fields: " ++ unName name
              else return PG.ElementKindEdge

      case kind of
        PG.ElementKindVertex -> do
            idAdapter <- projectionAdapter (schemaVertexIds schema) (Y.fromJust idProj) "id"
            return $ vertexCoder schema typ name (PG.VertexLabel labelStr) idAdapter propAdapters
        PG.ElementKindEdge -> do
            idAdapter <- projectionAdapter (schemaEdgeIds schema) (Y.fromJust idProj) "id"
            outAdapter <- projectionAdapter (schemaVertexIds schema) (Y.fromJust outProj) "out"
            inAdapter <- projectionAdapter (schemaVertexIds schema) (Y.fromJust inProj) "in"
            return $ edgeCoder schema typ name (PG.EdgeLabel labelStr) idAdapter outAdapter inAdapter propAdapters

    _ -> unexpected "record type" typ
  where
    findLabelString tname = case getTypeAnnotation "label" typ of
      Nothing -> pure $ unName tname
      Just labelTerm -> Expect.string labelTerm

    findId tname fields = do
      let idKey = "id"
      mid <- findField tname idKey fields
      case mid of
        Nothing -> fail $ "no " ++ idKey ++ "field"
        Just mi -> do
          spec <- decodeValueSpec $ Y.fromJust $ getTypeAnnotation idKey (fieldTypeType mi)
          return $ Just $ ProjectionSpec ProjectionTypeId mi spec

    findProjection tname key fields = do
      let idKey = key ++ "Id"
      mvalue <- findField tname key fields
      mid <- findField tname idKey fields
      case (mvalue, mid) of
        (Nothing, Nothing) -> pure Nothing
        (Just mv, Nothing) -> return $ Just $ ProjectionSpec ProjectionTypeValue mv ValueSpecValue
        (Nothing, Just mi) -> do
          spec <- decodeValueSpec $ Y.fromJust $ getTypeAnnotation idKey (fieldTypeType mi)
          return $ Just $ ProjectionSpec ProjectionTypeId mi spec
        _ -> fail $ "Found both " ++ key ++ " and " ++ idKey
      
    findField tname key fields = do
      let explicit = L.filter (\f -> Y.isJust $ getTypeAnnotation key $ fieldTypeType f) fields
      if L.length explicit > 0
        then fail $ "Multiple fields marked as '" ++ key ++ "' in record type " ++ unName tname
        else if L.null explicit
        then do
          let implicit = L.filter (\f -> key == unFieldName (fieldTypeName f)) fields
          if L.null implicit
            then return Nothing
            else return $ Just $ L.head implicit
        else return $ Just $ L.head explicit

    findPropertyFields fields = L.filter isPropField fields
      where
        isPropField field = not (isExcluded || isId || isOut || isIn)
          where
            isExcluded = hasAnnotation "exclude"
            isId = hasAnnotation "id" || hasName "id"
            isOut = hasAnnotation "out" || hasName "out"
            isIn = hasAnnotation "in" || hasName "in"
            hasAnnotation key = Y.isJust $ getTypeAnnotation key $ fieldTypeType field
            hasName fname = fieldTypeName field == FieldName fname

    projectionAdapter coder proj key = do
        case projectionSpecType proj of
          ProjectionTypeId -> do
            fun <- parseValueSpec $ projectionSpecValues proj
            let field = projectionSpecField proj
            return (fieldTypeName field, Adapter lossy (fieldTypeType field) () $ Coder (encode fun) decode)
          ProjectionTypeValue -> noValueProjection
      where
        encode fun term = do
          terms <- fun term
          case terms of
            [] -> fail $ key ++ "-projection did not resolve to a term"
            [t] -> coderDecode coder t
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

noValueProjection :: Flow s x
noValueProjection = fail "projection by value is not yet supported"

projectionId fields (fname, ad) = case M.lookup fname fields of
  Nothing -> fail $ "no " ++ unFieldName fname ++ " in record"
  Just t -> coderEncode (adapterCoder ad) t

propertyAdapter :: Show a => Schema s a t v e p -> FieldType a -> Flow s (PropAdapter s a t p)
propertyAdapter schema tfield = do
  let key = PG.PropertyKey $ unFieldName $ fieldTypeName tfield
  pt <- coderDecode (schemaPropertyTypes schema) $ fieldTypeType tfield
  let coder = Coder encode decode
        where
          encode dfield = do
            if fieldName dfield /= fieldTypeName tfield
              then unexpected ("field '" ++ unFieldName (fieldTypeName tfield) ++ "'") dfield
              else do
                value <- coderDecode (schemaPropertyValues schema) $ fieldTerm dfield
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
