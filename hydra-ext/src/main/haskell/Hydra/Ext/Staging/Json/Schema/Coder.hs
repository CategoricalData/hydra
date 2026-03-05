module Hydra.Ext.Staging.Json.Schema.Coder (
  JsonSchemaOptions(..),
  moduleToJsonSchema,
) where

import Hydra.Kernel
import qualified Hydra.Adapt.Terms as AdaptTerms
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Ext.Staging.Json.Schema.Serde as JsonSchemaSerde
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Json.Model as J
import qualified Hydra.Reflect as Reflect

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data JsonSchemaOptions = JsonSchemaOptions {
  jsonSchemaOptionsShortNames :: Bool
}

type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

moduleToJsonSchema :: JsonSchemaOptions -> Module.Module -> [Module.Definition] -> Context -> Graph.Graph -> Result (M.Map FilePath String)
moduleToJsonSchema opts mod defs cx g = do
  let (typeDefs, _termDefs) = partitionDefinitions defs
  docs <- constructModule cx g opts mod typeDefs
  return $ fmap JsonSchemaSerde.jsonSchemaDocumentToString docs

constructModule
  :: Context -> Graph.Graph
  -> JsonSchemaOptions
  -> Module.Module
  -> [Module.TypeDefinition]
  -> Result (M.Map FilePath JS.Document)
constructModule cx g opts mod typeDefs = M.fromList <$> CM.mapM toDocument typeDefs
  where
    -- Build a map from name to adapted type for lookups
    typeMap = M.fromList [(Module.typeDefinitionName td, Module.typeDefinitionType td) | td <- typeDefs]

    toDocument typeDef = typeDefToDocument (Module.typeDefinitionName typeDef) (Module.typeDefinitionType typeDef)

    typeDefToDocument rootName rootType = do
      -- Collect all type dependencies
      let depNames = S.toList $ Rewriting.typeDependencyNames True rootType
      let allNames = rootName : L.filter (/= rootName) depNames
      let allTypes = [Y.fromMaybe (Core.TypeVariable n) (M.lookup n typeMap) | n <- allNames]

      -- Apply short name substitution if needed
      let nameSubst = Rewriting.toShortNames allNames
      let types = Rewriting.substituteTypeVariables nameSubst <$> allTypes
      let names = substName nameSubst <$> allNames

      -- Encode all types
      schemas <- M.fromList <$> CM.zipWithM typeToKeywordDocumentPair names types

      return (nameToPath rootName, JS.Document Nothing (Just schemas) $
        JS.Schema [referenceRestriction $ substName nameSubst rootName])

    substName subst name = Y.fromMaybe name (M.lookup name subst)

    typeToKeywordDocumentPair name typ = do
      schema <- JS.Schema <$> encodeNamedType cx g name (excludeAnnotatedFields typ)
      return (JS.Keyword $ encodeName $ Core.Name $ Names.localNameOf name, schema)

    excludeAnnotatedFields = Rewriting.rewriteType $ \recurse typ -> case recurse typ of
        Core.TypeRecord (Core.RowType n fields) -> Core.TypeRecord $ Core.RowType n $ L.filter (not . isExcluded . Core.fieldTypeType) fields
        Core.TypeUnion (Core.RowType n fields) -> Core.TypeUnion $ Core.RowType n $ L.filter (not . isExcluded . Core.fieldTypeType) fields
        t -> t
      where
        isExcluded typ = Y.isJust $ Annotations.getTypeAnnotation Constants.key_exclude typ

    nameToPath name = Names.namespaceToFilePath Util.CaseConventionCamel (Module.FileExtension "json") (Module.Namespace $ nsPart ++ local)
      where
        (Module.QualifiedName mns local) = Names.qualifyName name
        nsPart = case mns of
          Nothing -> ""
          Just (Module.Namespace ns) -> ns ++ "/"

encodeField :: Context -> Graph.Graph -> Core.FieldType -> Result (JS.Keyword, JS.Schema)
encodeField cx g (Core.FieldType name typ) = do
  res <- encodeType cx g False typ
  return (JS.Keyword $ Core.unName name, JS.Schema res)

encodeName :: Core.Name -> String
encodeName = Formatting.nonAlnumToUnderscores . Core.unName

encodeNamedType :: Context -> Graph.Graph -> Core.Name -> Core.Type -> Result [JS.Restriction]
encodeNamedType cx g name typ = do
  res <- encodeType cx g False $ Rewriting.deannotateType typ
  return $ [JS.RestrictionTitle $ Core.unName name] ++ res

encodeType :: Context -> Graph.Graph -> Bool -> Core.Type -> Result [JS.Restriction]
encodeType cx g optional typ = case typ of
    Core.TypeAnnotated _ -> do
      res <- encodeType cx g optional $ Rewriting.deannotateType typ
      mdesc <- Annotations.getTypeDescription cx g typ
      let desc = Y.maybe [] (\d -> [JS.RestrictionDescription d]) mdesc
      return $ desc ++ res
    Core.TypeEither (Core.EitherType lt rt) -> do
      leftRes <- encodeType cx g False lt
      rightRes <- encodeType cx g False rt
      let leftField = (JS.Keyword "left", JS.Schema leftRes)
      let rightField = (JS.Keyword "right", JS.Schema rightRes)
      let leftSchema = JS.Schema [
            JS.RestrictionType $ JS.TypeSingle JS.TypeNameObject,
            JS.RestrictionObject $ JS.ObjectRestrictionProperties $ M.fromList [leftField],
            JS.RestrictionObject $ JS.ObjectRestrictionRequired [JS.Keyword "left"],
            JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsAny False]
      let rightSchema = JS.Schema [
            JS.RestrictionType $ JS.TypeSingle JS.TypeNameObject,
            JS.RestrictionObject $ JS.ObjectRestrictionProperties $ M.fromList [rightField],
            JS.RestrictionObject $ JS.ObjectRestrictionRequired [JS.Keyword "right"],
            JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsAny False]
      return [JS.RestrictionMultiple $ JS.MultipleRestrictionOneOf [leftSchema, rightSchema]]
    Core.TypePair (Core.PairType ft st) -> do
      firstRes <- encodeType cx g False ft
      secondRes <- encodeType cx g False st
      let firstField = (JS.Keyword "first", JS.Schema firstRes)
      let secondField = (JS.Keyword "second", JS.Schema secondRes)
      let props = M.fromList [firstField, secondField]
      let reqs = [JS.Keyword "first", JS.Keyword "second"]
      return $ jsType JS.TypeNameObject ++
        [JS.RestrictionObject $ JS.ObjectRestrictionProperties props,
         JS.RestrictionObject $ JS.ObjectRestrictionRequired reqs,
         JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsAny False]
    Core.TypeList lt -> do
      elSchema <- JS.Schema <$> encodeType cx g False lt
      let arrayRes = [JS.RestrictionArray $ JS.ArrayRestrictionItems $ JS.ItemsSameItems elSchema]
      pure $ jsType JS.TypeNameArray ++ arrayRes
    Core.TypeLiteral lt -> case lt of
      Core.LiteralTypeBinary -> pure $ jsType JS.TypeNameString
      Core.LiteralTypeBoolean -> pure $ jsType JS.TypeNameBoolean
      Core.LiteralTypeFloat ft -> pure $ jsType JS.TypeNameNumber
      Core.LiteralTypeInteger ft -> pure $ jsType JS.TypeNameInteger
      Core.LiteralTypeString -> pure $ jsType JS.TypeNameString
    Core.TypeMap (Core.MapType _ vt) -> do -- Note: we assume that keys are strings
      vschema <- JS.Schema <$> encodeType cx g False vt
      let objRes = [JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsSchema vschema]
      pure $ jsType JS.TypeNameObject ++ objRes
    Core.TypeMaybe t -> encodeType cx g True t -- Note: nested optionals are lost
    Core.TypeRecord rt -> encodeRecordOrUnion False rt
    Core.TypeUnion rt -> if L.null simpleFields
        then asRecord rt
        else do
          recSchema <- JS.Schema <$> asRecord (rt {Core.rowTypeFields = nonsimpleFields})
          let names = Core.fieldTypeName <$> simpleFields
          return [JS.RestrictionMultiple $ JS.MultipleRestrictionOneOf $ [recSchema, toSimpleSchema names]]
      where
        toSimpleSchema names = JS.Schema [
          JS.RestrictionType $ JS.TypeSingle $ JS.TypeNameString,
          JS.RestrictionMultiple $ JS.MultipleRestrictionEnum (J.ValueString . Core.unName <$> names)]
        asRecord rt = encodeRecordOrUnion True $ AdaptTerms.unionTypeToRecordType rt
        (simpleFields, nonsimpleFields) = L.partition isSimple $ Core.rowTypeFields rt
        isSimple (Core.FieldType _ ft) = Schemas.isUnitType $ Rewriting.deannotateType ft
    Core.TypeSet st -> do
      elSchema <- JS.Schema <$> encodeType cx g False st
      let arrayRes = [JS.RestrictionArray $ JS.ArrayRestrictionItems $ JS.ItemsSameItems elSchema]
      pure $ jsType JS.TypeNameArray ++ arrayRes
    Core.TypeVariable name -> pure [referenceRestriction name]
    Core.TypeWrap (Core.WrappedType _ inner) -> encodeType cx g optional inner
    _ -> err cx $ "unsupported type variant: " ++ show (Reflect.typeVariant typ)
  where
    encodeRecordOrUnion union (Core.RowType _ fields) = do
        props <- M.fromList <$> CM.mapM (encodeField cx g) fields
        let objRes = [JS.RestrictionObject $ JS.ObjectRestrictionProperties props]
        let reqRes = if L.null reqs then [] else [JS.RestrictionObject $ JS.ObjectRestrictionRequired reqs]
        pure $ jsType JS.TypeNameObject ++ objRes ++ reqRes ++ cardRes
      where
        reqs = Y.catMaybes $ fmap ifReq fields
        ifReq field = if isRequiredField field then Just (JS.Keyword $ Core.unName $ Core.fieldTypeName field) else Nothing
        cardRes = JS.RestrictionObject <$>
            [JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsAny False]
            ++ if union then [
              JS.ObjectRestrictionMinProperties 1,
              JS.ObjectRestrictionMaxProperties 1] else []
    jsType tname = [JS.RestrictionType jst]
      where
        jst = if optional
          then JS.TypeMultiple [tname, JS.TypeNameNull]
          else JS.TypeSingle tname

isRequiredField :: Core.FieldType -> Bool
isRequiredField (Core.FieldType _ typ) = case Rewriting.deannotateType typ of
  Core.TypeMaybe _ -> False
  _ -> True

referenceRestriction :: Core.Name -> JS.Restriction
referenceRestriction name = JS.RestrictionReference $ JS.SchemaReference $ "#/$defs/" ++ encodeName name
