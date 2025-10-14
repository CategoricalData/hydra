module Hydra.Ext.Staging.Json.Schema.Coder (
  JsonSchemaOptions(..),
  moduleToJsonSchemaFiles,
) where

import qualified Hydra.Adapt.Modules as AdaptModules
import qualified Hydra.Adapt.Terms as AdaptTerms
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Annotations as DslAnnotations
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Ext.Org.Json.Schema.Language as JsonSchemaLanguage
import qualified Hydra.Ext.Staging.Json.Schema.Serde as JsonSchemaSerde
import qualified Hydra.Variants as Variants
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Json as J
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data JsonSchemaOptions = JsonSchemaOptions {
  jsonSchemaOptionsShortNames :: Bool
}

constructModule
  :: JsonSchemaOptions
  -> Module.Module
  -> M.Map Core.Type (Compute.Coder Graph.Graph Graph.Graph Core.Term ())
  -> [(Core.Binding, Core.TypeApplicationTerm)]
  -> Compute.Flow Graph.Graph (M.Map FilePath JS.Document)
constructModule opts mod coders pairs = M.fromList <$> CM.mapM toDocument pairs
  where
    toDocument (el, tt@(Core.TypeApplicationTerm term typ)) = if Annotations.isNativeType el
      then typeTermToDocument (Core.bindingName el)
      else Monads.fail $ "mapping of non-type elements to JSON Schema is not yet supported: " ++ Core.unName (Core.bindingName el)

    typeTermToDocument rootName = do
      nt <- Schemas.typeDependencies False excludeAnnotatedFields rootName
      let names = M.keys nt
      let nameSubst = Rewriting.toShortNames names
      let types = Rewriting.substituteTypeVariables nameSubst <$> M.elems nt
      schemas <- M.fromList <$> (CM.zipWithM typeToKeywordDocumentPair (substName nameSubst <$> names) types)

      return (nameToPath rootName, JS.Document Nothing (Just schemas) $
        JS.Schema [referenceRestriction $ substName nameSubst rootName])

    substName subst name = Y.fromMaybe name (M.lookup name subst)

    excludeAnnotatedFields = Rewriting.rewriteType $ \recurse typ -> case recurse typ of
        Core.TypeRecord (Core.RowType n fields) -> Core.TypeRecord $ Core.RowType n $ L.filter (not . isExcluded . Core.fieldTypeType) fields
        Core.TypeUnion (Core.RowType n fields) -> Core.TypeUnion $ Core.RowType n $ L.filter (not . isExcluded . Core.fieldTypeType) fields
        t -> t
      where
        isExcluded typ = Y.isJust $ Annotations.getTypeAnnotation Constants.key_exclude typ

    typeToKeywordDocumentPair name typ = do
      g <- Monads.getState
      atyp <- Compute.adapterTarget <$> (Monads.withState (Coders.AdapterContext g JsonSchemaLanguage.jsonSchemaLanguage M.empty) $ AdaptTerms.termAdapter typ)
      schema <- JS.Schema <$> encodeNamedType name atyp
      return (JS.Keyword $ encodeName $ Core.Name $ Names.localNameOf name, schema)

    nameToPath name = Names.namespaceToFilePath Mantle.CaseConventionCamel (Module.FileExtension "json") (Module.Namespace $ nsPart ++ local)
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
    Core.TypeOptional t -> encodeType True t -- Note: nested optionals are lost
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
        isSimple (Core.FieldType _ ft) = EncodeCore.isUnitType $ Rewriting.deannotateType ft
    Core.TypeVariable name -> Monads.pure [referenceRestriction name]
    _ -> Monads.fail $ "unsupported type variant: " ++ show (Variants.typeVariant typ)
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
  Core.TypeOptional _ -> False
  _ -> True

moduleToJsonSchemaDocuments :: JsonSchemaOptions -> Module.Module -> Compute.Flow Graph.Graph (M.Map FilePath JS.Document)
moduleToJsonSchemaDocuments opts mod = AdaptModules.transformModule JsonSchemaLanguage.jsonSchemaLanguage encodeTerm (constructModule opts) mod

moduleToJsonSchemaFiles :: JsonSchemaOptions -> Module.Module -> Compute.Flow Graph.Graph (M.Map FilePath String)
moduleToJsonSchemaFiles opts mod = do
  files <- moduleToJsonSchemaDocuments opts mod
  return $ fmap JsonSchemaSerde.jsonSchemaDocumentToString files

referenceRestriction :: Core.Name -> JS.Restriction
referenceRestriction name = JS.RestrictionReference $ JS.SchemaReference $ "#/$defs/" ++ encodeName name
