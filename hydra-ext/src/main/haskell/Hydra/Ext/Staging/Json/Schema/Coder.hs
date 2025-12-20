module Hydra.Ext.Staging.Json.Schema.Coder (
  JsonSchemaOptions(..),
  moduleToJsonSchema,
) where

import Hydra.Kernel
import qualified Hydra.Adapt.Terms as AdaptTerms
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Ext.Staging.Json.Schema.Serde as JsonSchemaSerde
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Json as J
import qualified Hydra.Reflect as Reflect

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data JsonSchemaOptions = JsonSchemaOptions {
  jsonSchemaOptionsShortNames :: Bool
}

moduleToJsonSchema :: JsonSchemaOptions -> Module.Module -> [Module.Definition] -> Compute.Flow Graph.Graph (M.Map FilePath String)
moduleToJsonSchema opts mod defs = do
  let (typeDefs, _termDefs) = partitionDefinitions defs
  docs <- constructModule opts mod typeDefs
  return $ fmap JsonSchemaSerde.jsonSchemaDocumentToString docs

constructModule
  :: JsonSchemaOptions
  -> Module.Module
  -> [Module.TypeDefinition]
  -> Compute.Flow Graph.Graph (M.Map FilePath JS.Document)
constructModule opts mod typeDefs = M.fromList <$> CM.mapM toDocument typeDefs
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
      schema <- JS.Schema <$> encodeNamedType name (excludeAnnotatedFields typ)
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

encodeField :: Core.FieldType -> Compute.Flow Graph.Graph (JS.Keyword, JS.Schema)
encodeField (Core.FieldType name typ) = do
  res <- encodeType False typ
  return (JS.Keyword $ Core.unName name, JS.Schema res)

encodeName :: Core.Name -> String
encodeName = Formatting.nonAlnumToUnderscores . Core.unName

encodeTerm :: Core.Term -> Compute.Flow Graph.Graph ()
encodeTerm term = Monads.fail "not yet implemented"

encodeNamedType :: Core.Name -> Core.Type -> Compute.Flow Graph.Graph [JS.Restriction]
encodeNamedType name typ = do
  res <- encodeType False $ Rewriting.deannotateType typ
  return $ [JS.RestrictionTitle $ Core.unName name] ++ res

encodeType :: Bool -> Core.Type -> Compute.Flow Graph.Graph [JS.Restriction]
encodeType optional typ = case typ of
    Core.TypeAnnotated _ -> do
      res <- encodeType optional $ Rewriting.deannotateType typ
      mdesc <- Annotations.getTypeDescription typ
      let desc = Y.maybe [] (\d -> [JS.RestrictionDescription d]) mdesc
      return $ desc ++ res
    Core.TypeEither (Core.EitherType lt rt) -> do
      leftRes <- encodeType False lt
      rightRes <- encodeType False rt
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
      firstRes <- encodeType False ft
      secondRes <- encodeType False st
      let firstField = (JS.Keyword "first", JS.Schema firstRes)
      let secondField = (JS.Keyword "second", JS.Schema secondRes)
      let props = M.fromList [firstField, secondField]
      let reqs = [JS.Keyword "first", JS.Keyword "second"]
      return $ jsType JS.TypeNameObject ++
        [JS.RestrictionObject $ JS.ObjectRestrictionProperties props,
         JS.RestrictionObject $ JS.ObjectRestrictionRequired reqs,
         JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsAny False]
    Core.TypeList lt -> do
      elSchema <- JS.Schema <$> encodeType False lt
      let arrayRes = [JS.RestrictionArray $ JS.ArrayRestrictionItems $ JS.ItemsSameItems elSchema]
      Monads.pure $ jsType JS.TypeNameArray ++ arrayRes
    Core.TypeLiteral lt -> case lt of
      Core.LiteralTypeBinary -> Monads.pure $ jsType JS.TypeNameString
      Core.LiteralTypeBoolean -> Monads.pure $ jsType JS.TypeNameBoolean
      Core.LiteralTypeFloat ft -> Monads.pure $ jsType JS.TypeNameNumber
      Core.LiteralTypeInteger ft -> Monads.pure $ jsType JS.TypeNameInteger
      Core.LiteralTypeString -> Monads.pure $ jsType JS.TypeNameString
    Core.TypeMap (Core.MapType _ vt) -> do -- Note: we assume that keys are strings
      vschema <- JS.Schema <$> encodeType False vt
      let objRes = [JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsSchema vschema]
      Monads.pure $ jsType JS.TypeNameObject ++ objRes
    Core.TypeMaybe t -> encodeType True t -- Note: nested optionals are lost
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
      elSchema <- JS.Schema <$> encodeType False st
      let arrayRes = [JS.RestrictionArray $ JS.ArrayRestrictionItems $ JS.ItemsSameItems elSchema]
      Monads.pure $ jsType JS.TypeNameArray ++ arrayRes
    Core.TypeVariable name -> Monads.pure [referenceRestriction name]
    Core.TypeWrap (Core.WrappedType _ inner) -> encodeType optional inner
    _ -> Monads.fail $ "unsupported type variant: " ++ show (Reflect.typeVariant typ)
  where
    encodeRecordOrUnion union (Core.RowType _ fields) = do
        props <- M.fromList <$> CM.mapM encodeField fields
        let objRes = [JS.RestrictionObject $ JS.ObjectRestrictionProperties props]
        let reqRes = if L.null reqs then [] else [JS.RestrictionObject $ JS.ObjectRestrictionRequired reqs]
        Monads.pure $ jsType JS.TypeNameObject ++ objRes ++ reqRes ++ cardRes
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
