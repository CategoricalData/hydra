module Hydra.Ext.Pegasus.Coder (printGraph) where

import Hydra.Adapter
import Hydra.Adapters.Term
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreLanguage
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Monads
import Hydra.Rewriting
import Hydra.Adapters.Coders
import Hydra.Util.Formatting
import Hydra.Ext.Pegasus.Language
import qualified Hydra.Ext.Pegasus.Pdl as PDL
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Util.Codetree.Script
import Hydra.Ext.Pegasus.Serde

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


printGraph :: (Ord m, Read m, Show m) => Graph m -> GraphFlow m (M.Map FilePath String)
printGraph g = do
  sf <- moduleToPegasusSchema g
  let s = printExpr $ parenthesize $ exprSchemaFile sf
  return $ M.fromList [(graphNameToFilePath False (FileExtension "pdl") $ graphName g, s)]

constructModule :: (Ord m, Read m, Show m)
  => Graph m
  -> M.Map (Type m) (Coder (Context m) (Term m) ())
  -> [(Element m, TypedTerm m)]
  -> GraphFlow m PDL.SchemaFile
constructModule g coders pairs = do
    let ns = pdlNameForGraph g
    let pkg = Nothing
    let imports = [] -- TODO
    sortedPairs <- case (topologicalSortElements $ fst <$> pairs) of
      Nothing -> fail $ "types form a cycle (unsupported in PDL)"
      Just sorted -> pure $ Y.catMaybes $ fmap (\n -> M.lookup n pairByName) sorted
    schemas <- CM.mapM toSchema sortedPairs
    return $ PDL.SchemaFile ns pkg imports schemas
  where
    pairByName = L.foldl (\m p@(el, tt) -> M.insert (elementName el) p m) M.empty pairs
    aliases = importAliasesForGraph g
    toSchema (el, TypedTerm typ term) = if stripType typ == TypeNominal _Type
      then decodeType term >>= typeToSchema el
      else fail $ "mapping of non-type elements to PDL is not yet supported: " ++ show typ
    typeToSchema el typ = do
      let qname = pdlNameForElement aliases False $ elementName el
      res <- encodeAdaptedType aliases typ
      let ptype = case res of
            Left schema -> PDL.NamedSchema_TypeTyperef schema
            Right t -> t
      cx <- getState
      r <- annotationClassTermDescription (contextAnnotations cx) $ elementData el
      let anns = doc r
      return $ PDL.NamedSchema qname ptype anns

moduleToPegasusSchema :: (Ord m, Read m, Show m) => Graph m -> GraphFlow m PDL.SchemaFile
moduleToPegasusSchema g = graphToExternalModule language (encodeTerm aliases) constructModule g
  where
    aliases = importAliasesForGraph g

doc :: Y.Maybe String -> PDL.Annotations
doc s = PDL.Annotations s False

encodeAdaptedType :: (Ord m, Read m, Show m)
  => M.Map GraphName String -> Type m
  -> GraphFlow m (Either PDL.Schema PDL.NamedSchema_Type)
encodeAdaptedType aliases typ = do
  cx <- getState
  let acx = AdapterContext cx hydraCoreLanguage language
  ad <- withState acx $ termAdapter typ
  encodeType aliases $ adapterTarget ad

encodeTerm :: (Eq m, Ord m, Read m, Show m) => M.Map GraphName String -> Term m -> GraphFlow m ()
encodeTerm aliases term = fail "not yet implemented"

encodeType :: (Eq m, Show m) => M.Map GraphName String -> Type m -> GraphFlow m (Either PDL.Schema PDL.NamedSchema_Type)
encodeType aliases typ = case stripType typ of
    TypeList lt -> Left . PDL.SchemaArray <$> encode lt
    TypeLiteral lt -> Left . PDL.SchemaPrimitive <$> case lt of
      LiteralTypeBinary -> pure PDL.PrimitiveTypeBytes
      LiteralTypeBoolean -> pure PDL.PrimitiveTypeBoolean
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat32 -> pure PDL.PrimitiveTypeFloat
        FloatTypeFloat64 -> pure PDL.PrimitiveTypeDouble
        _ -> fail $ "unexpected floating-point type: " ++ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeInt32 -> pure PDL.PrimitiveTypeInt
        IntegerTypeInt64 -> pure PDL.PrimitiveTypeLong
        _ -> fail $ "unexpected integer type: " ++ show it
      LiteralTypeString -> pure PDL.PrimitiveTypeString
    TypeMap (MapType kt vt) -> Left . PDL.SchemaMap <$> encode vt -- note: we simply assume string as a key type
    TypeNominal name -> pure $ Left $ PDL.SchemaNamed $ pdlNameForElement aliases True name
    TypeOptional ot -> fail $ "optionals unexpected at top level"
    TypeRecord (RowType _ fields) -> do
      let includes = []
      rfields <- CM.mapM encodeRecordField fields
      return $ Right $ PDL.NamedSchema_TypeRecord $ PDL.RecordSchema rfields includes
    TypeUnion (RowType _ fields) -> if isEnum
        then do
          fs <- CM.mapM encodeEnumField fields
          return $ Right $ PDL.NamedSchema_TypeEnum $ PDL.EnumSchema fs
        else Left . PDL.SchemaUnion . PDL.UnionSchema <$> CM.mapM encodeUnionField fields
      where
        isEnum = L.foldl (\b t -> b && stripType t == Types.unit) True $ fmap fieldTypeType fields
    _ -> fail $ "unexpected type: " ++ show typ
  where
    encode t = case stripType t of
      TypeRecord (RowType _ []) -> encode Types.int32 -- special case for the unit type
      _ -> do
        res <- encodeType aliases t
        case res of
          Left schema -> pure schema
          Right _ -> fail $ "type resolved to an unsupported nested named schema: " ++ show t
    encodeRecordField (FieldType (FieldName name) typ) = do
      anns <- getAnns typ
      (schema, optional) <- encodePossiblyOptionalType typ
      return PDL.RecordField {
        PDL.recordFieldName = PDL.FieldName name,
        PDL.recordFieldValue = schema,
        PDL.recordFieldOptional = optional,
        PDL.recordFieldDefault = Nothing,
        PDL.recordFieldAnnotations = anns}
    encodeUnionField (FieldType (FieldName name) typ) = do
      anns <- getAnns typ
      (s, optional) <- encodePossiblyOptionalType typ
      let schema = if optional
          then PDL.SchemaUnion $ PDL.UnionSchema (simpleUnionMember <$> [PDL.SchemaNull, s])
          else s
      return PDL.UnionMember {
        PDL.unionMemberAlias = Just $ PDL.FieldName name,
        PDL.unionMemberValue = schema,
        PDL.unionMemberAnnotations = anns}
    encodeEnumField (FieldType (FieldName name) typ) = do
      anns <- getAnns typ
      return PDL.EnumField {
        PDL.enumFieldName = PDL.EnumFieldName $ convertCase CaseCamel CaseUpperSnake name,
        PDL.enumFieldAnnotations = anns}
    encodePossiblyOptionalType typ = case stripType typ of
      TypeOptional ot -> do
        t <- encode ot
        return (t, True)
      _ -> do
        t <- encode typ
        return (t, False)
    getAnns typ = do
      cx <- getState
      r <- annotationClassTypeDescription (contextAnnotations cx) typ
      return $ doc r

importAliasesForGraph g = M.empty -- TODO

noAnnotations :: PDL.Annotations
noAnnotations = PDL.Annotations Nothing False

pdlNameForElement :: M.Map GraphName String -> Bool -> Name -> PDL.QualifiedName
pdlNameForElement aliases withNs name = PDL.QualifiedName (PDL.Name local)
    $ if withNs
      then PDL.Namespace . slashesToDots <$> alias
      else Nothing
  where
    (ns, local) = toQname name
    alias = M.lookup ns aliases

pdlNameForGraph :: Graph m -> PDL.Namespace
pdlNameForGraph = PDL.Namespace . slashesToDots . h . graphName
  where
    h (GraphName n) = n

simpleUnionMember :: PDL.Schema -> PDL.UnionMember
simpleUnionMember schema = PDL.UnionMember Nothing schema noAnnotations

slashesToDots :: String -> String
slashesToDots = fmap (\c -> if c == '/' then '.' else c)
