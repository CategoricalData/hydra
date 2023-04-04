module Hydra.Langs.Graphql.Coder where -- (printGraph) where

import Hydra.Kernel
import Hydra.Langs.Graphql.Language
import Hydra.Langs.Graphql.Serde
import qualified Hydra.Langs.Graphql.Syntax as G
import Hydra.Tools.Script

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


printModule :: (Ord a, Read a, Show a) => Module a -> GraphFlow a (M.Map FilePath String)
printModule mod = do
    files <- moduleToGraphqlSchemas mod
    return $ M.fromList (mapPair <$> M.toList files)
  where
    mapPair (path, sf) = (path, printExpr $ parenthesize $ exprDocument sf)

moduleToGraphqlSchemas :: (Ord a, Read a, Show a) => Module a -> GraphFlow a (M.Map FilePath G.Document)
moduleToGraphqlSchemas mod = transformModule graphqlLanguage encodeTerm constructModule mod

constructModule :: (Ord a, Read a, Show a)
  => Module a
  -> M.Map (Type a) (Coder (Graph a) (Graph a) (Term a) ())
  -> [(Element a, TypedTerm a)]
  -> GraphFlow a (M.Map FilePath G.Document)
constructModule mod coders pairs = do
    tdefs <- CM.mapM toTypeDef pairs
    let doc = G.Document $ (G.DefinitionTypeSystem . G.TypeSystemDefinitionOrExtensionDefinition . G.TypeSystemDefinitionType) <$> tdefs
    return $ M.fromList [(filePath, doc)]
  where
    filePath = namespaceToFilePath False (FileExtension "sdl") (moduleNamespace mod)
    toTypeDef (el, TypedTerm typ term) = do
      if isType typ
        then epsilonDecodeType term >>= encodeNamedType el
        else fail $ "mapping of non-type elements to GraphQL is not yet supported: " ++ unName (elementName el)

descriptionFromType :: Type a -> Maybe G.Description
descriptionFromType typ = Nothing -- TODO

encodeEnumFieldType :: FieldType a -> GraphFlow a G.EnumValueDefinition
encodeEnumFieldType ft = pure $ G.EnumValueDefinition {
  G.enumValueDefinitionDescription = descriptionFromType $ fieldTypeType ft,
  G.enumValueDefinitionEnumValue = encodeEnumFieldName $ fieldTypeName ft,
  G.enumValueDefinitionDirectives = Nothing}

encodeEnumFieldName :: FieldName -> G.EnumValue
encodeEnumFieldName = G.EnumValue . G.Name . unFieldName

encodeFieldName :: FieldName -> G.Name
encodeFieldName = G.Name . unFieldName

encodeFieldType :: Show a => FieldType a -> GraphFlow a G.FieldDefinition
encodeFieldType ft = do
  gtype <- encodeType $ fieldTypeType ft
  return G.FieldDefinition {
    G.fieldDefinitionDescription = descriptionFromType $ fieldTypeType ft,
    G.fieldDefinitionName = encodeFieldName $ fieldTypeName ft,
    G.fieldDefinitionArgumentsDefinition = Nothing,
    G.fieldDefinitionType = gtype,
    G.fieldDefinitionDirectives = Nothing}

encodeLiteralType :: LiteralType -> GraphFlow a G.NamedType
encodeLiteralType lt = G.NamedType . G.Name <$> case lt of
  LiteralTypeBoolean -> pure "Boolean"
  LiteralTypeFloat ft -> case ft of
    FloatTypeFloat64 -> pure "Float"
    _ -> unexpected "64-bit float type" ft
  LiteralTypeInteger it -> case it of
    IntegerTypeInt32 -> pure "Int"
    _ -> unexpected "32-bit signed integer type" it
  LiteralTypeString -> pure "String"
  _ -> unexpected "GraphQL-compatible literal type" lt

encodeNamedType :: Show a => Element a -> Type a -> GraphFlow a G.TypeDefinition
encodeNamedType el typ = case stripType typ of
  TypeRecord rt -> do
    gfields <- CM.mapM encodeFieldType $ rowTypeFields rt
    return $ G.TypeDefinitionObject $ G.ObjectTypeDefinition {
      G.objectTypeDefinitionDescription = descriptionFromType typ,
      G.objectTypeDefinitionName = encodeTypeName $ elementName el,
      G.objectTypeDefinitionImplementsInterfaces = Nothing,
      G.objectTypeDefinitionDirectives = Nothing,
      G.objectTypeDefinitionFieldsDefinition = Just $ G.FieldsDefinition gfields}
  TypeUnion rt -> do
    values <- CM.mapM encodeEnumFieldType $ rowTypeFields rt
    return $ G.TypeDefinitionEnum $ G.EnumTypeDefinition {
      G.enumTypeDefinitionDescription = descriptionFromType typ,
      G.enumTypeDefinitionName = encodeTypeName $ elementName el,
      G.enumTypeDefinitionDirectives = Nothing,
      G.enumTypeDefinitionEnumValuesDefinition = Just $ G.EnumValuesDefinition values}
  _ -> unexpected "record or union type" typ

encodeTerm :: (Eq a, Ord a, Read a, Show a) => Term a -> GraphFlow a ()
encodeTerm term = fail "not yet implemented"

encodeType :: Show a => Type a -> GraphFlow a G.Type
encodeType typ = case stripType typ of
    TypeOptional et -> case stripType et of
        TypeList et -> G.TypeList . G.ListType <$> encodeType et
        TypeLiteral lt -> G.TypeNamed <$> encodeLiteralType lt
        TypeRecord rt -> forRowType rt
        TypeUnion rt -> forRowType rt
        TypeWrap name -> forName name
        _ -> unexpected "GraphQL-compatible type" typ
      where
        forName = pure . G.TypeNamed . G.NamedType . encodeTypeName
        forRowType = forName . rowTypeTypeName
    t -> G.TypeNonNull <$> nonnull t
  where
    nonnull t = case stripType t of
        TypeList et -> G.NonNullTypeList . G.ListType <$> encodeType et
        TypeLiteral lt -> G.NonNullTypeNamed <$> encodeLiteralType lt
        TypeRecord rt -> forRowType rt
        TypeUnion rt -> forRowType rt
        TypeWrap name -> forName name
        _ -> unexpected "GraphQL-compatible non-null type" t
      where
        forName = pure . G.NonNullTypeNamed . G.NamedType . encodeTypeName
        forRowType = forName . rowTypeTypeName
     
encodeTypeName :: Name -> G.Name
encodeTypeName name = G.Name $ localNameOfEager name
