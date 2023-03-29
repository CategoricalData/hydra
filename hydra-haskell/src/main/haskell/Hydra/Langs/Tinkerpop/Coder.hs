module Hydra.Langs.Tinkerpop.Coder (
  elementCoder,
) where
  
import Hydra.Kernel
import Hydra.Langs.Tinkerpop.Mappings
import qualified Hydra.Langs.Tinkerpop.V3 as TP
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


type ElementAdapter s a t v e p = Adapter s s (Type a) (TP.ElementType t) (Term a) (TP.Element v e p)

type PropAdapter s a t p = Adapter s s (FieldType a) (TP.PropertyType t) (Field a) (TP.Property p)

type VertexIdAdapter s a v = Adapter s s (Type a) () (Term a) v

check b err = if b then pure () else err
checkRecordName expected actual = check (actual == expected) $
  unexpected ("record of type " ++ unName expected) ("record of type " ++ unName actual)

edgeCoder :: Show a
  => Schema s a t v e p -> Type a -> Name
  -> TP.EdgeLabel -> Maybe (FieldType a) -> VertexIdAdapter s a v -> VertexIdAdapter s a v -> [PropAdapter s a t p]
  -> ElementAdapter s a t v e p
edgeCoder schema typ tname label idField outAdapter inAdapter propAdapters = Adapter lossy typ (TP.ElementTypeEdge et) coder
  where
    et = TP.EdgeType label outt int $ propertyTypes propAdapters
    outt = TP.VertexType (TP.VertexLabel "TODO") M.empty -- TODO
    int = TP.VertexType (TP.VertexLabel "TODO") M.empty -- TODO
    coder = Coder encode decode
      where
        encode term = do
          case stripTerm term of
            TermRecord (Record tname' fields) -> do
                checkRecordName tname tname'
                id <- coderDecode (schemaEdgeIds schema) $ Terms.string "DONTCARE" -- TODO
                props <- encodeProperties fields propAdapters
                outId <- coderDecode (schemaVertexIds schema) $ Terms.string "DONTCARE" -- TODO
                inId <- coderDecode (schemaVertexIds schema) $ Terms.string "DONTCARE" -- TODO
                return $ TP.ElementEdge $ TP.Edge label id outId inId props
            _ -> unexpected "record" term
        decode el = noDecoding "edge"

elementCoder :: Schema s Kv t v e p -> Type Kv -> Flow s (ElementAdapter s Kv t v e p)
elementCoder schema typ = case stripType typ of
    TypeRecord (RowType name _ fields) -> do
      labelStr <- findLabelString name

      idField <- fieldField name "id" fields
      outField <- fieldField name "out" fields
      inField <- fieldField name "in" fields
      
      let propFields = findPropertyFields fields
      propAdapters <- CM.mapM (propertyAdapter schema) propFields
      
      kind <- case getTypeAnnotation "kind" typ of
        Nothing -> if Y.isNothing outField || Y.isNothing inField
          then pure TP.ElementKindVertex
          else pure TP.ElementKindEdge
        Just kindTerm -> do
          s <- Expect.string kindTerm
          case s of
            "vertex" -> return TP.ElementKindVertex
            "edge" -> if Y.isNothing outField || Y.isNothing inField
              then fail $ "Record type marked as an edge type, but missing 'out' and/or 'in' fields: " ++ unName name
              else return TP.ElementKindEdge

      case kind of
        TP.ElementKindVertex -> pure $ vertexCoder schema typ name (TP.VertexLabel labelStr) idField propAdapters
        TP.ElementKindEdge -> pure $ edgeCoder schema typ name (TP.EdgeLabel labelStr) idField outAdapter inAdapter propAdapters
          where
            outAdapter = Adapter lossy (fieldTypeType $ Y.fromJust outField) () $ Coder encode decode
              where
                encode = coderDecode (schemaVertexIds schema)
                decode _ = noDecoding "edge 'out'"
            inAdapter = Adapter lossy (fieldTypeType $ Y.fromJust inField) () $ Coder encode decode
              where
                encode = coderDecode (schemaVertexIds schema)
                decode _ = noDecoding "edge 'in'"
    _ -> unexpected "record type" typ
  where
    findLabelString tname = case getTypeAnnotation "label" typ of
      Nothing -> pure $ unName tname
      Just labelTerm -> Expect.string labelTerm

    fieldField tname key fields = do
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

encodeProperties :: [Field a] -> [PropAdapter s a t p] -> Flow s (M.Map TP.PropertyKey p)
encodeProperties fields adapters = do
  props <- CM.mapM (encodeProperty $ fieldMap fields) adapters
  return $ M.fromList $ fmap (\(TP.Property key val) -> (key, val)) props

encodeProperty :: M.Map FieldName (Term a) -> PropAdapter s a t p -> Flow s (TP.Property p)
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

propertyAdapter :: Show a => Schema s a t v e p -> FieldType a -> Flow s (PropAdapter s a t p)
propertyAdapter schema tfield = do
  let key = TP.PropertyKey $ unFieldName $ fieldTypeName tfield
  pt <- coderDecode (schemaPropertyTypes schema) $ fieldTypeType tfield
  let coder = Coder encode decode
        where
          encode dfield = do
            if fieldName dfield /= fieldTypeName tfield
              then unexpected ("field '" ++ unFieldName (fieldTypeName tfield) ++ "'") dfield
              else do
                value <- coderDecode (schemaPropertyValues schema) $ fieldTerm dfield
                return $ TP.Property key value
          decode _ = noDecoding "property"
  return $ Adapter lossy tfield (TP.PropertyType key pt) coder

propertyTypes propAdapters = M.fromList $
  fmap (\a -> (TP.propertyTypeKey $ adapterTarget a, TP.propertyTypeValue $ adapterTarget a)) propAdapters

vertexCoder :: Show a
  => Schema s a t v e p -> Type a -> Name
  -> TP.VertexLabel -> Maybe (FieldType a) -> [PropAdapter s a t p]
  -> ElementAdapter s a t v e p
vertexCoder schema typ tname label idField propAdapters = Adapter lossy typ (TP.ElementTypeVertex vt) coder
  where
    vt = TP.VertexType label $ propertyTypes propAdapters
    coder = Coder encode decode
      where
        encode term = do
          case stripTerm term of
            TermRecord (Record tname' fields) -> do
                checkRecordName tname tname'
                id <- coderDecode (schemaVertexIds schema) $ Terms.string "DONTCARE" -- TODO
                props <- encodeProperties fields propAdapters
                return $ TP.ElementVertex $ TP.Vertex label id props
            _ -> unexpected "record" term
        decode el = noDecoding "vertex"
