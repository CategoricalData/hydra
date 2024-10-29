module Hydra.Ext.Json.Schema.Coder (moduleToJsonSchemaFiles) where

import Hydra.Kernel
import Hydra.TermAdapters
import Hydra.Adapters
import Hydra.Ext.Json.Schema.Language
import Hydra.Tools.Serialization
import Hydra.Ext.Json.Schema.Serde
import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


constructModule
  :: Module
  -> M.Map Type (Coder Graph Graph Term ())
  -> [(Element, TypedTerm)]
  -> Flow Graph (M.Map FilePath JS.Document)
constructModule mod coders pairs = M.fromList <$> CM.mapM toDocument pairs
  where
    toDocument (el, TypedTerm term typ) = if isType typ
      then typeTermToDocument (elementName el)
      else fail $ "mapping of non-type elements to JSON Schema is not yet supported: " ++ unName (elementName el)

    typeTermToDocument rootName = do
      names <- M.keys <$> typeDependencies rootName
      terms <- fmap elementData <$> (CM.mapM requireElement names)
      types <- CM.mapM coreDecodeType terms
      schemas <- M.fromList <$> (CM.zipWithM typeToKeywordDocumentPair names types)

      return (nameToPath rootName, JS.Document Nothing (Just schemas) $ JS.Schema [referenceRestriction rootName])

    typeToKeywordDocumentPair name typ = do
      g <- getState
      atyp <- adapterTarget <$> (withState (AdapterContext g jsonSchemaLanguage M.empty) $ termAdapter typ)
      schema <- JS.Schema <$> encodeNamedType name atyp
      return (JS.Keyword $ encodeName name, schema)

    nameToPath name = namespaceToFilePath False (FileExtension "json") (Namespace $ nsPart ++ local)
      where
        (QualifiedName mns local) = qualifyNameLazy name
        nsPart = case mns of
          Nothing -> ""
          Just (Namespace ns) -> ns ++ "/"

encodeField :: FieldType -> Flow Graph (JS.Keyword, JS.Schema)
encodeField (FieldType name typ) = do
  res <- encodeType False typ
  return (JS.Keyword $ unName name, JS.Schema res)

encodeName :: Name -> String
encodeName = nonAlnumToUnderscores . unName

encodeTerm :: Term -> Flow Graph ()
encodeTerm term = fail "not yet implemented"

encodeNamedType :: Name -> Type -> Flow Graph [JS.Restriction]
encodeNamedType name typ = do
  res <- encodeType False $ stripType typ
  return $ [JS.RestrictionTitle $ unName name] ++ res

encodeType :: Bool -> Type -> Flow Graph [JS.Restriction]
encodeType optional typ = case typ of
    TypeAnnotated _ -> do
      res <- encodeType optional $ stripType typ
      mdesc <- getTypeDescription typ
      let desc = Y.maybe [] (\d -> [JS.RestrictionDescription d]) mdesc
      return $ desc ++ res
    TypeList lt -> do
      elSchema <- JS.Schema <$> encodeType False lt
      let arrayRes = [JS.RestrictionArray $ JS.ArrayRestrictionItems $ JS.ItemsSameItems elSchema]
      pure $ jsType JS.TypeNameArray ++ arrayRes
    TypeLiteral lt -> case lt of
      LiteralTypeBinary -> pure $ jsType JS.TypeNameString
      LiteralTypeBoolean -> pure $ jsType JS.TypeNameBoolean
      LiteralTypeFloat ft -> pure $ jsType JS.TypeNameNumber
      LiteralTypeInteger ft -> pure $ jsType JS.TypeNameInteger
      LiteralTypeString -> pure $ jsType JS.TypeNameString
    TypeMap (MapType _ vt) -> do -- Note: we assume that keys are strings
      vschema <- JS.Schema <$> encodeType False vt
      let objRes = [JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsSchema vschema]
      pure $ jsType JS.TypeNameObject ++ objRes
    TypeOptional t -> encodeType True t -- Note: nested optionals are lost
    TypeRecord rt -> encodeRecordOrUnion False rt
    TypeUnion rt -> encodeRecordOrUnion True $ unionTypeToRecordType rt
    TypeVariable name -> pure [referenceRestriction name]
    _ -> fail $ "unsupported type variant: " ++ show (typeVariant typ)
  where
    encodeRecordOrUnion union (RowType _ fields) = do
        props <- M.fromList <$> CM.mapM encodeField fields
        let objRes = [JS.RestrictionObject $ JS.ObjectRestrictionProperties props]
        let reqRes = if L.null reqs then [] else [JS.RestrictionObject $ JS.ObjectRestrictionRequired reqs]
        pure $ jsType JS.TypeNameObject ++ objRes ++ reqRes ++ cardRes
      where
        reqs = Y.catMaybes $ fmap ifReq fields
        ifReq field = if isRequiredField field then Just (JS.Keyword $ unName $ fieldTypeName field) else Nothing
        totalReq = if union then 1 else L.length $ L.filter isRequiredField fields
        total = if union then 1 else L.length fields
        cardRes = JS.RestrictionObject <$>
            [JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsAny False]
            ++ minRes
            ++ maxRes
          where
            minRes = if totalReq > 0 then [JS.ObjectRestrictionMinProperties totalReq] else []
            maxRes = [JS.ObjectRestrictionMaxProperties total]
    jsType tname = [JS.RestrictionType jst]
      where
        jst = if optional
          then JS.TypeMultiple [tname, JS.TypeNameNull]
          else JS.TypeSingle tname

isRequiredField :: FieldType -> Bool
isRequiredField (FieldType _ typ) = case stripType typ of
  TypeOptional _ -> False
  _ -> True

moduleToJsonSchemaDocuments :: Module -> Flow Graph (M.Map FilePath JS.Document)
moduleToJsonSchemaDocuments mod = transformModule jsonSchemaLanguage encodeTerm constructModule mod

moduleToJsonSchemaFiles :: Module -> Flow Graph (M.Map FilePath String)
moduleToJsonSchemaFiles mod = do
  files <- moduleToJsonSchemaDocuments mod
  return $ fmap jsonSchemaDocumentToString files

referenceRestriction :: Name -> JS.Restriction
referenceRestriction name = JS.RestrictionReference $ JS.SchemaReference $ "#/$defs/" ++ encodeName name
