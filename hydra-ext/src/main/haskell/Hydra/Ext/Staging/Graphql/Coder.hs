module Hydra.Ext.Staging.Graphql.Coder (moduleToGraphql) where

import Hydra.Kernel
import Hydra.Ext.Staging.Graphql.Language
import Hydra.Ext.Staging.Graphql.Serde
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Ext.Org.Graphql.Syntax as G
import Hydra.Formatting

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Prefixes = M.Map Namespace String

-- TODO
requireTermType :: Term -> Flow s Type
requireTermType term = fail $ "update me: types are derived using typeOf now, rather than from explicitly typed terms"

moduleToGraphql :: Module -> Flow Graph (M.Map FilePath String)
moduleToGraphql mod = do
    files <- moduleToGraphqlSchemas mod
    return $ M.fromList (mapPair <$> M.toList files)
  where
    mapPair (path, sf) = (path, printExpr $ parenthesize $ exprDocument sf)

moduleToGraphqlSchemas :: Module -> Flow Graph (M.Map FilePath G.Document)
moduleToGraphqlSchemas mod = transformModule graphqlLanguage encodeTerm constructModule mod

constructModule :: Module
  -> M.Map Type (Coder Graph Graph Term ())
  -> [(Binding, TypedTerm)]
  -> Flow Graph (M.Map FilePath G.Document)
constructModule mod coders pairs = do
    -- Gather all dependencies because GraphQL does not support imports (in a standard way)
    withDeps <- elementsWithDependencies $ fst <$> pairs
    -- Qualify the names of dependencies with prefixes, so as to avoid name collisions
    let prefixes = findPrefixes withDeps
    -- Elements to GraphQL type definitions
    tdefs <- CM.mapM (toTypeDef prefixes) withDeps
    let doc = G.Document $ (G.DefinitionTypeSystem . G.TypeSystemDefinitionOrExtensionDefinition . G.TypeSystemDefinitionType) <$> tdefs
    return $ M.fromList [(filePath, doc)]
  where
    filePath = namespaceToFilePath CaseConventionCamel (FileExtension "graphql") (moduleNamespace mod)
    findPrefixes els = M.fromList $ toPair <$> namespaces
      where
        namespaces = L.nub $ (Y.fromJust . namespaceOf . bindingName) <$> els
        toPair ns = (ns, if ns == moduleNamespace mod then "" else (sanitizeWithUnderscores S.empty (unNamespace ns)) ++ "_")
    toTypeDef prefixes el = do
      typ <- requireTermType term
      if isNativeType el
        then DecodeCore.type_ (bindingTerm el) >>= encodeNamedType prefixes el
        else fail $ "mapping of non-type elements to GraphQL is not yet supported: " ++ unName (bindingName el)
     where
       term = bindingTerm el

descriptionFromType :: Type -> Flow Graph (Maybe G.Description)
descriptionFromType typ = do
  mval <- getTypeDescription typ
  return $ G.Description . G.StringValue <$> mval

encodeEnumFieldType :: FieldType -> Flow Graph G.EnumValueDefinition
encodeEnumFieldType ft = do
  desc <- descriptionFromType $ fieldTypeType ft
  return G.EnumValueDefinition {
    G.enumValueDefinitionDescription = desc,
    G.enumValueDefinitionEnumValue = encodeEnumFieldName $ fieldTypeName ft,
    G.enumValueDefinitionDirectives = Nothing}

encodeEnumFieldName :: Name -> G.EnumValue
encodeEnumFieldName = G.EnumValue . G.Name . sanitize . unName

encodeFieldName :: Name -> G.Name
encodeFieldName = G.Name . sanitize . unName

encodeFieldType :: Prefixes -> FieldType -> Flow Graph G.FieldDefinition
encodeFieldType prefixes ft = do
  gtype <- encodeType prefixes $ fieldTypeType ft
  desc <- descriptionFromType $ fieldTypeType ft
  return G.FieldDefinition {
    G.fieldDefinitionDescription = desc,
    G.fieldDefinitionName = encodeFieldName $ fieldTypeName ft,
    G.fieldDefinitionArgumentsDefinition = Nothing,
    G.fieldDefinitionType = gtype,
    G.fieldDefinitionDirectives = Nothing}

encodeLiteralType :: LiteralType -> Flow Graph G.NamedType
encodeLiteralType lt = G.NamedType . G.Name <$> case lt of
  LiteralTypeBoolean -> pure "Boolean"
  LiteralTypeFloat ft -> case ft of
    FloatTypeFloat64 -> pure "Float"
    _ -> unexpected "64-bit float type" $ show ft
  LiteralTypeInteger it -> case it of
    IntegerTypeInt32 -> pure "Int"
    _ -> unexpected "32-bit signed integer type" $ show it
  LiteralTypeString -> pure "String"
  _ -> unexpected "GraphQL-compatible literal type" $ show lt

encodeNamedType :: Prefixes -> Binding -> Type -> Flow Graph G.TypeDefinition
encodeNamedType prefixes el typ = do
    g <- getState
    let cx = AdapterContext g graphqlLanguage M.empty
    ad <- withState cx $ termAdapter typ
    case deannotateType (adapterTarget ad) of
      TypeRecord rt -> do
        gfields <- CM.mapM (encodeFieldType prefixes) $ rowTypeFields rt
        desc <- descriptionFromType typ
        return $ G.TypeDefinitionObject $ G.ObjectTypeDefinition {
          G.objectTypeDefinitionDescription = desc,
          G.objectTypeDefinitionName = encodeTypeName prefixes $ bindingName el,
          G.objectTypeDefinitionImplementsInterfaces = Nothing,
          G.objectTypeDefinitionDirectives = Nothing,
          G.objectTypeDefinitionFieldsDefinition = Just $ G.FieldsDefinition gfields}
      TypeUnion rt -> do
        values <- CM.mapM encodeEnumFieldType $ rowTypeFields rt
        desc <- descriptionFromType typ
        return $ G.TypeDefinitionEnum $ G.EnumTypeDefinition {
          G.enumTypeDefinitionDescription = desc,
          G.enumTypeDefinitionName = encodeTypeName prefixes $ bindingName el,
          G.enumTypeDefinitionDirectives = Nothing,
          G.enumTypeDefinitionEnumValuesDefinition = Just $ G.EnumValuesDefinition values}
      TypeList _ -> wrapAsRecord
      TypeLiteral _ -> wrapAsRecord
      TypeVariable _ -> wrapAsRecord
      t -> unexpected "record or union type" $ show t
  where
    wrapAsRecord = encodeNamedType prefixes el $ TypeRecord $ RowType (bindingName el) [
      FieldType (Name "value") typ]

encodeTerm :: Term -> Flow Graph ()
encodeTerm term = fail "not yet implemented"

encodeType :: Prefixes -> Type -> Flow Graph G.Type
encodeType prefixes typ = case deannotateType typ of
    TypeOptional et -> case deannotateType et of
        TypeList et -> G.TypeList . G.ListType <$> encodeType prefixes et
        TypeLiteral lt -> G.TypeNamed <$> encodeLiteralType lt
        TypeRecord rt -> forRowType rt
        TypeUnion rt -> forRowType rt
--         TypeWrap name -> forName name
        TypeVariable name -> forName name
        t -> unexpected "GraphQL-compatible type" $ show t
      where
        forName = pure . G.TypeNamed . G.NamedType . encodeTypeName prefixes
        forRowType = forName . rowTypeTypeName
    t -> G.TypeNonNull <$> nonnull t
  where
    nonnull t = case deannotateType t of
        TypeList et -> G.NonNullTypeList . G.ListType <$> encodeType prefixes et
        TypeLiteral lt -> G.NonNullTypeNamed <$> encodeLiteralType lt
        TypeRecord rt -> forRowType rt
        TypeUnion rt -> forRowType rt
        TypeVariable name -> forName name
--         TypeWrap name -> forName name
        _ -> unexpected "GraphQL-compatible non-null type" $ show t
      where
        forName = pure . G.NonNullTypeNamed . G.NamedType . encodeTypeName prefixes
        forRowType = forName . rowTypeTypeName

encodeTypeName :: Prefixes -> Name -> G.Name
encodeTypeName prefixes name = G.Name $ (prefix ++ sanitize local)
  where
    prefix = Y.fromMaybe "UNKNOWN" $ M.lookup ns prefixes
    QualifiedName (Just ns) local = qualifyName name

sanitize :: String -> String
sanitize = sanitizeWithUnderscores graphqlReservedWords
