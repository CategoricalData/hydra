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

type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

unexpectedE :: Context -> String -> String -> Result a
unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found


-- | New simple adapter version that works with definitions directly
moduleToGraphql :: Module -> [Definition] -> Context -> Graph -> Result (M.Map FilePath String)
moduleToGraphql mod defs cx g = do
    let (typeDefs, _termDefs) = partitionDefinitions defs
    -- Build prefixes for namespace qualification
    let prefixes = findPrefixes typeDefs
    -- Convert type definitions to GraphQL type definitions
    gtdefs <- CM.mapM (encodeTypeDefinition cx g prefixes) typeDefs
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
encodeTypeDefinition :: Context -> Graph -> Prefixes -> TypeDefinition -> Result G.TypeDefinition
encodeTypeDefinition cx g prefixes tdef = encodeNamedType cx g prefixes (typeDefinitionName tdef) (typeDefinitionType tdef)

descriptionFromType :: Context -> Graph -> Type -> Result (Maybe G.Description)
descriptionFromType cx g typ = do
  mval <- getTypeDescription cx g typ
  return $ G.Description . G.StringValue <$> mval

encodeEnumFieldType :: Context -> Graph -> FieldType -> Result G.EnumValueDefinition
encodeEnumFieldType cx g ft = do
  desc <- descriptionFromType cx g $ fieldTypeType ft
  return G.EnumValueDefinition {
    G.enumValueDefinitionDescription = desc,
    G.enumValueDefinitionEnumValue = encodeEnumFieldName $ fieldTypeName ft,
    G.enumValueDefinitionDirectives = Nothing}

encodeEnumFieldName :: Name -> G.EnumValue
encodeEnumFieldName = G.EnumValue . G.Name . sanitize . unName

encodeFieldName :: Name -> G.Name
encodeFieldName = G.Name . sanitize . unName

encodeFieldType :: Context -> Graph -> Prefixes -> FieldType -> Result G.FieldDefinition
encodeFieldType cx g prefixes ft = do
  gtype <- encodeType cx g prefixes $ fieldTypeType ft
  desc <- descriptionFromType cx g $ fieldTypeType ft
  return G.FieldDefinition {
    G.fieldDefinitionDescription = desc,
    G.fieldDefinitionName = encodeFieldName $ fieldTypeName ft,
    G.fieldDefinitionArgumentsDefinition = Nothing,
    G.fieldDefinitionType = gtype,
    G.fieldDefinitionDirectives = Nothing}

encodeLiteralType :: Context -> LiteralType -> Result G.NamedType
encodeLiteralType cx lt = G.NamedType . G.Name <$> case lt of
  LiteralTypeBoolean -> pure "Boolean"
  LiteralTypeFloat ft -> case ft of
    FloatTypeFloat64 -> pure "Float"
    _ -> unexpectedE cx "64-bit float type" $ show ft
  LiteralTypeInteger it -> case it of
    IntegerTypeInt32 -> pure "Int"
    _ -> unexpectedE cx "32-bit signed integer type" $ show it
  LiteralTypeString -> pure "String"
  _ -> unexpectedE cx "GraphQL-compatible literal type" $ show lt

-- | Encode a named type to a GraphQL type definition.
-- The type has already been adapted by schemaGraphToDefinitions.
encodeNamedType :: Context -> Graph -> Prefixes -> Name -> Type -> Result G.TypeDefinition
encodeNamedType cx g prefixes name typ = case deannotateType typ of
      TypeRecord rt -> do
        gfields <- CM.mapM (encodeFieldType cx g prefixes) $ rowTypeFields rt
        desc <- descriptionFromType cx g typ
        return $ G.TypeDefinitionObject $ G.ObjectTypeDefinition {
          G.objectTypeDefinitionDescription = desc,
          G.objectTypeDefinitionName = encodeTypeName prefixes name,
          G.objectTypeDefinitionImplementsInterfaces = Nothing,
          G.objectTypeDefinitionDirectives = Nothing,
          G.objectTypeDefinitionFieldsDefinition = Just $ G.FieldsDefinition gfields}
      TypeUnion rt -> do
        values <- CM.mapM (encodeEnumFieldType cx g) $ rowTypeFields rt
        desc <- descriptionFromType cx g typ
        return $ G.TypeDefinitionEnum $ G.EnumTypeDefinition {
          G.enumTypeDefinitionDescription = desc,
          G.enumTypeDefinitionName = encodeTypeName prefixes name,
          G.enumTypeDefinitionDirectives = Nothing,
          G.enumTypeDefinitionEnumValuesDefinition = Just $ G.EnumValuesDefinition values}
      TypeEither (EitherType lt rt) -> do
        -- Either types become records with optional left/right fields (exactly one should be present)
        encodeNamedType cx g prefixes name $ TypeRecord $ RowType name [
          FieldType (Name "left") (Types.optional lt),
          FieldType (Name "right") (Types.optional rt)]
      TypePair (PairType ft st) -> do
        -- Pair types become records with first/second fields
        encodeNamedType cx g prefixes name $ TypeRecord $ RowType name [
          FieldType (Name "first") ft,
          FieldType (Name "second") st]
      TypeList lt -> wrapAsRecord lt
      TypeSet st -> wrapAsRecord (TypeList st) -- Sets become lists
      TypeMap (MapType _ vt) -> wrapAsRecord (TypeList vt) -- Maps become lists of values
      TypeLiteral lt -> wrapAsRecord (TypeLiteral lt)
      TypeVariable vn -> wrapAsRecord (TypeVariable vn)
      TypeWrap (WrappedType _ inner) -> wrapAsRecord inner
      t -> unexpectedE cx "record or union type" $ show t
  where
    -- Create a record wrapper with "value" field containing the inner type
    wrapAsRecord innerTyp = encodeNamedType cx g prefixes name $ TypeRecord $ RowType name [
      FieldType (Name "value") innerTyp]

encodeType :: Context -> Graph -> Prefixes -> Type -> Result G.Type
encodeType cx g prefixes typ = case deannotateType typ of
    TypeMaybe et -> case deannotateType et of
        TypeList et -> G.TypeList . G.ListType <$> encodeType cx g prefixes et
        TypeSet st -> G.TypeList . G.ListType <$> encodeType cx g prefixes st -- Sets become lists
        TypeMap (MapType _ vt) -> G.TypeList . G.ListType <$> encodeType cx g prefixes vt -- Maps become lists of values
        TypeLiteral lt -> G.TypeNamed <$> encodeLiteralType cx lt
        TypeRecord rt -> forRowType rt
        TypeUnion rt -> forRowType rt
        TypeWrap (WrappedType n _) -> forName n
        TypeVariable n -> forName n
        t -> unexpectedE cx "GraphQL-compatible type" $ show t
      where
        forName = pure . G.TypeNamed . G.NamedType . encodeTypeName prefixes
        forRowType = forName . rowTypeTypeName
    t -> G.TypeNonNull <$> nonnull t
  where
    nonnull t = case deannotateType t of
        TypeList et -> G.NonNullTypeList . G.ListType <$> encodeType cx g prefixes et
        TypeSet st -> G.NonNullTypeList . G.ListType <$> encodeType cx g prefixes st -- Sets become lists
        TypeMap (MapType _ vt) -> G.NonNullTypeList . G.ListType <$> encodeType cx g prefixes vt -- Maps become lists of values
        TypeLiteral lt -> G.NonNullTypeNamed <$> encodeLiteralType cx lt
        TypeRecord rt -> forRowType rt
        TypeUnion rt -> forRowType rt
        TypeVariable n -> forName n
        TypeWrap (WrappedType n _) -> forName n
        _ -> unexpectedE cx "GraphQL-compatible non-null type" $ show t
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
