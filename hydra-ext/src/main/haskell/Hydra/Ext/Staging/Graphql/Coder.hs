module Hydra.Ext.Staging.Graphql.Coder (moduleToGraphql) where

import Hydra.Kernel
import Hydra.Ext.Staging.Graphql.Language
import Hydra.Ext.Staging.Graphql.Serde
import qualified Hydra.Ext.Org.Graphql.Syntax as G
import qualified Hydra.Dsl.Types as Types
import Hydra.Formatting

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Prefixes = M.Map Namespace String


-- | New simple adapter version that works with definitions directly
moduleToGraphql :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToGraphql mod defs = withTrace ("encode module to GraphQL: " ++ unNamespace (moduleNamespace mod)) $ do
    let (typeDefs, _termDefs) = partitionDefinitions defs
    -- Build prefixes for namespace qualification
    let prefixes = findPrefixes typeDefs
    -- Convert type definitions to GraphQL type definitions
    gtdefs <- CM.mapM (encodeTypeDefinition prefixes) typeDefs
    let doc = G.Document $ (G.DefinitionTypeSystem . G.TypeSystemDefinitionOrExtensionDefinition . G.TypeSystemDefinitionType) <$> gtdefs
    let content = printExpr $ parenthesize $ exprDocument doc
    return $ M.fromList [(filePath, content)]
  where
    filePath = namespaceToFilePath CaseConventionCamel (FileExtension "graphql") (moduleNamespace mod)
    findPrefixes tdefs = M.fromList $ toPair <$> namespaces
      where
        namespaces = L.nub $ Y.catMaybes $ (namespaceOf . typeDefinitionName) <$> tdefs
        toPair ns = (ns, if ns == moduleNamespace mod then "" else (sanitizeWithUnderscores S.empty (unNamespace ns)) ++ "_")

-- | Encode a TypeDefinition to a GraphQL TypeDefinition
encodeTypeDefinition :: Prefixes -> TypeDefinition -> Flow Graph G.TypeDefinition
encodeTypeDefinition prefixes tdef = encodeNamedType prefixes (typeDefinitionName tdef) (typeDefinitionType tdef)

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

-- | Encode a named type to a GraphQL type definition.
-- The type has already been adapted by schemaGraphToDefinitions.
encodeNamedType :: Prefixes -> Name -> Type -> Flow Graph G.TypeDefinition
encodeNamedType prefixes name typ = case deannotateType typ of
      TypeRecord rt -> do
        gfields <- CM.mapM (encodeFieldType prefixes) $ rowTypeFields rt
        desc <- descriptionFromType typ
        return $ G.TypeDefinitionObject $ G.ObjectTypeDefinition {
          G.objectTypeDefinitionDescription = desc,
          G.objectTypeDefinitionName = encodeTypeName prefixes name,
          G.objectTypeDefinitionImplementsInterfaces = Nothing,
          G.objectTypeDefinitionDirectives = Nothing,
          G.objectTypeDefinitionFieldsDefinition = Just $ G.FieldsDefinition gfields}
      TypeUnion rt -> do
        values <- CM.mapM encodeEnumFieldType $ rowTypeFields rt
        desc <- descriptionFromType typ
        return $ G.TypeDefinitionEnum $ G.EnumTypeDefinition {
          G.enumTypeDefinitionDescription = desc,
          G.enumTypeDefinitionName = encodeTypeName prefixes name,
          G.enumTypeDefinitionDirectives = Nothing,
          G.enumTypeDefinitionEnumValuesDefinition = Just $ G.EnumValuesDefinition values}
      TypeEither (EitherType lt rt) -> do
        -- Either types become records with optional left/right fields (exactly one should be present)
        encodeNamedType prefixes name $ TypeRecord $ RowType name [
          FieldType (Name "left") (Types.optional lt),
          FieldType (Name "right") (Types.optional rt)]
      TypePair (PairType ft st) -> do
        -- Pair types become records with first/second fields
        encodeNamedType prefixes name $ TypeRecord $ RowType name [
          FieldType (Name "first") ft,
          FieldType (Name "second") st]
      TypeList lt -> wrapAsRecord lt
      TypeSet st -> wrapAsRecord (TypeList st) -- Sets become lists
      TypeMap (MapType _ vt) -> wrapAsRecord (TypeList vt) -- Maps become lists of values
      TypeLiteral lt -> wrapAsRecord (TypeLiteral lt)
      TypeVariable vn -> wrapAsRecord (TypeVariable vn)
      TypeWrap (WrappedType _ inner) -> wrapAsRecord inner
      t -> unexpected "record or union type" $ show t
  where
    -- Create a record wrapper with "value" field containing the inner type
    wrapAsRecord innerTyp = encodeNamedType prefixes name $ TypeRecord $ RowType name [
      FieldType (Name "value") innerTyp]

encodeType :: Prefixes -> Type -> Flow Graph G.Type
encodeType prefixes typ = case deannotateType typ of
    TypeMaybe et -> case deannotateType et of
        TypeList et -> G.TypeList . G.ListType <$> encodeType prefixes et
        TypeSet st -> G.TypeList . G.ListType <$> encodeType prefixes st -- Sets become lists
        TypeMap (MapType _ vt) -> G.TypeList . G.ListType <$> encodeType prefixes vt -- Maps become lists of values
        TypeLiteral lt -> G.TypeNamed <$> encodeLiteralType lt
        TypeRecord rt -> forRowType rt
        TypeUnion rt -> forRowType rt
        TypeWrap (WrappedType n _) -> forName n
        TypeVariable n -> forName n
        t -> unexpected "GraphQL-compatible type" $ show t
      where
        forName = pure . G.TypeNamed . G.NamedType . encodeTypeName prefixes
        forRowType = forName . rowTypeTypeName
    t -> G.TypeNonNull <$> nonnull t
  where
    nonnull t = case deannotateType t of
        TypeList et -> G.NonNullTypeList . G.ListType <$> encodeType prefixes et
        TypeSet st -> G.NonNullTypeList . G.ListType <$> encodeType prefixes st -- Sets become lists
        TypeMap (MapType _ vt) -> G.NonNullTypeList . G.ListType <$> encodeType prefixes vt -- Maps become lists of values
        TypeLiteral lt -> G.NonNullTypeNamed <$> encodeLiteralType lt
        TypeRecord rt -> forRowType rt
        TypeUnion rt -> forRowType rt
        TypeVariable n -> forName n
        TypeWrap (WrappedType n _) -> forName n
        _ -> unexpected "GraphQL-compatible non-null type" $ show t
      where
        forName = pure . G.NonNullTypeNamed . G.NamedType . encodeTypeName prefixes
        forRowType = forName . rowTypeTypeName

encodeTypeName :: Prefixes -> Name -> G.Name
encodeTypeName prefixes name = G.Name $ (prefix ++ sanitize local)
  where
    QualifiedName mns local = qualifyName name
    prefix = case mns of
      Just ns -> Y.fromMaybe "" $ M.lookup ns prefixes
      Nothing -> ""

sanitize :: String -> String
sanitize = sanitizeWithUnderscores graphqlReservedWords
