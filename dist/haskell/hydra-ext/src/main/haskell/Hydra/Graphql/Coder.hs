-- Note: this is an automatically generated file. Do not edit.

-- | GraphQL code generator: converts Hydra modules to GraphQL schema definitions

module Hydra.Graphql.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Core as Core
import qualified Hydra.Environment as Environment
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Graphql.Language as Language
import qualified Hydra.Graphql.Serde as Serde
import qualified Hydra.Graphql.Syntax as Syntax
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

descriptionFromType :: t0 -> Graph.Graph -> Core.Type -> Either Errors.Error (Maybe Syntax.Description)
descriptionFromType cx g typ =
    Eithers.map (\mval -> Maybes.map (\s -> Syntax.Description (Syntax.StringValue s)) mval) (Annotations.getTypeDescription cx g typ)

encodeEnumFieldName :: Core.Name -> Syntax.EnumValue
encodeEnumFieldName name = Syntax.EnumValue (Syntax.Name (sanitize (Core.unName name)))

encodeEnumFieldType :: t0 -> Graph.Graph -> Core.FieldType -> Either Errors.Error Syntax.EnumValueDefinition
encodeEnumFieldType cx g ft =
    Eithers.bind (descriptionFromType cx g (Core.fieldTypeType ft)) (\desc -> Right (Syntax.EnumValueDefinition {
      Syntax.enumValueDefinitionDescription = desc,
      Syntax.enumValueDefinitionEnumValue = (encodeEnumFieldName (Core.fieldTypeName ft)),
      Syntax.enumValueDefinitionDirectives = Nothing}))

encodeFieldName :: Core.Name -> Syntax.Name
encodeFieldName name = Syntax.Name (sanitize (Core.unName name))

encodeFieldType :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Core.FieldType -> Either Errors.Error Syntax.FieldDefinition
encodeFieldType cx g prefixes ft =
    Eithers.bind (encodeType cx g prefixes (Core.fieldTypeType ft)) (\gtype -> Eithers.bind (descriptionFromType cx g (Core.fieldTypeType ft)) (\desc -> Right (Syntax.FieldDefinition {
      Syntax.fieldDefinitionDescription = desc,
      Syntax.fieldDefinitionName = (encodeFieldName (Core.fieldTypeName ft)),
      Syntax.fieldDefinitionArgumentsDefinition = Nothing,
      Syntax.fieldDefinitionType = gtype,
      Syntax.fieldDefinitionDirectives = Nothing})))

encodeLiteralType :: t0 -> Core.LiteralType -> Either Errors.Error Syntax.NamedType
encodeLiteralType cx lt =
    case lt of
      Core.LiteralTypeBoolean -> Right (Syntax.NamedType (Syntax.Name "Boolean"))
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeFloat64 -> Right (Syntax.NamedType (Syntax.Name "Float"))
        _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected 64-bit float type, found: " (ShowCore.floatType v0))))
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeInt32 -> Right (Syntax.NamedType (Syntax.Name "Int"))
        _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected 32-bit signed integer type, found: " (ShowCore.integerType v0))))
      Core.LiteralTypeString -> Right (Syntax.NamedType (Syntax.Name "String"))
      _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected GraphQL-compatible literal type, found: " (ShowCore.literalType lt))))

encodeNamedType :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Core.Name -> Core.Type -> Either Errors.Error Syntax.TypeDefinition
encodeNamedType cx g prefixes name typ =
    case (Strip.deannotateType typ) of
      Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (\f -> encodeFieldType cx g prefixes f) v0) (\gfields -> Eithers.bind (descriptionFromType cx g typ) (\desc -> Right (Syntax.TypeDefinitionObject (Syntax.ObjectTypeDefinition {
        Syntax.objectTypeDefinitionDescription = desc,
        Syntax.objectTypeDefinitionName = (encodeTypeName prefixes name),
        Syntax.objectTypeDefinitionImplementsInterfaces = Nothing,
        Syntax.objectTypeDefinitionDirectives = Nothing,
        Syntax.objectTypeDefinitionFieldsDefinition = (Just (Syntax.FieldsDefinition gfields))}))))
      Core.TypeUnion v0 -> Logic.ifElse (Predicates.isEnumRowType v0) (Eithers.bind (Eithers.mapList (\f -> encodeEnumFieldType cx g f) v0) (\values -> Eithers.bind (descriptionFromType cx g typ) (\desc -> Right (Syntax.TypeDefinitionEnum (Syntax.EnumTypeDefinition {
        Syntax.enumTypeDefinitionDescription = desc,
        Syntax.enumTypeDefinitionName = (encodeTypeName prefixes name),
        Syntax.enumTypeDefinitionDirectives = Nothing,
        Syntax.enumTypeDefinitionEnumValuesDefinition = (Just (Syntax.EnumValuesDefinition values))}))))) (Eithers.bind (Eithers.mapList (\f -> encodeUnionFieldType cx g prefixes f) v0) (\gfields -> Eithers.bind (descriptionFromType cx g typ) (\desc -> Right (Syntax.TypeDefinitionObject (Syntax.ObjectTypeDefinition {
        Syntax.objectTypeDefinitionDescription = desc,
        Syntax.objectTypeDefinitionName = (encodeTypeName prefixes name),
        Syntax.objectTypeDefinitionImplementsInterfaces = Nothing,
        Syntax.objectTypeDefinitionDirectives = Nothing,
        Syntax.objectTypeDefinitionFieldsDefinition = (Just (Syntax.FieldsDefinition gfields))})))))
      Core.TypeEither v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "left"),
          Core.fieldTypeType = (Core.TypeMaybe (Core.eitherTypeLeft v0))},
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "right"),
          Core.fieldTypeType = (Core.TypeMaybe (Core.eitherTypeRight v0))}])
      Core.TypePair v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "first"),
          Core.fieldTypeType = (Core.pairTypeFirst v0)},
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "second"),
          Core.fieldTypeType = (Core.pairTypeSecond v0)}])
      Core.TypeList v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = (Core.TypeList v0)}])
      Core.TypeSet v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = (Core.TypeList v0)}])
      Core.TypeMap v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "key"),
          Core.fieldTypeType = (Core.mapTypeKeys v0)},
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = (Core.mapTypeValues v0)}])
      Core.TypeLiteral v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = (Core.TypeLiteral v0)}])
      Core.TypeVariable v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = (Core.TypeVariable v0)}])
      Core.TypeWrap v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = v0}])
      Core.TypeUnit -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}])
      Core.TypeForall v0 -> encodeNamedType cx g prefixes name (Core.forallTypeBody v0)
      Core.TypeApplication v0 -> encodeNamedType cx g prefixes name (Core.applicationTypeFunction v0)
      Core.TypeFunction v0 -> encodeNamedType cx g prefixes name (Core.TypeRecord [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "domain"),
          Core.fieldTypeType = (Core.functionTypeDomain v0)},
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "codomain"),
          Core.fieldTypeType = (Core.functionTypeCodomain v0)}])
      _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected record or union type, found: " (ShowCore.type_ typ))))

encodeType :: t0 -> t1 -> M.Map Packaging.Namespace String -> Core.Type -> Either Errors.Error Syntax.Type
encodeType cx g prefixes typ =
    case (Strip.deannotateType typ) of
      Core.TypeMaybe v0 -> case (Strip.deannotateType v0) of
        Core.TypeList v1 -> Eithers.map (\gt -> Syntax.TypeList (Syntax.ListType gt)) (encodeType cx g prefixes v1)
        Core.TypeSet v1 -> Eithers.map (\gt -> Syntax.TypeList (Syntax.ListType gt)) (encodeType cx g prefixes v1)
        Core.TypeMap v1 -> Eithers.map (\gt -> Syntax.TypeList (Syntax.ListType gt)) (encodeType cx g prefixes (Core.mapTypeValues v1))
        Core.TypeLiteral v1 -> Eithers.map (\nt -> Syntax.TypeNamed nt) (encodeLiteralType cx v1)
        Core.TypePair _ -> Right (Syntax.TypeNamed (Syntax.NamedType (encodeTypeName prefixes (Core.Name "hydra.util.Pair"))))
        Core.TypeEither _ -> Right (Syntax.TypeNamed (Syntax.NamedType (encodeTypeName prefixes (Core.Name "hydra.util.Either"))))
        Core.TypeRecord _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous record type"))
        Core.TypeUnion _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous union type"))
        Core.TypeWrap v1 -> encodeType cx g prefixes (Core.TypeMaybe v1)
        Core.TypeVariable v1 -> Right (Syntax.TypeNamed (Syntax.NamedType (encodeTypeName prefixes v1)))
        Core.TypeForall v1 -> encodeType cx g prefixes (Core.TypeMaybe (Core.forallTypeBody v1))
        Core.TypeApplication v1 -> encodeType cx g prefixes (Core.TypeMaybe (Core.applicationTypeFunction v1))
        Core.TypeFunction _ -> Right (Syntax.TypeNamed (Syntax.NamedType (Syntax.Name "String")))
        Core.TypeUnit -> Right (Syntax.TypeNamed (Syntax.NamedType (Syntax.Name "Boolean")))
        _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected GraphQL-compatible type, found: " (ShowCore.type_ v0))))
      Core.TypeList v0 -> Eithers.map (\gt -> Syntax.TypeNonNull (Syntax.NonNullTypeList (Syntax.ListType gt))) (encodeType cx g prefixes v0)
      Core.TypeSet v0 -> Eithers.map (\gt -> Syntax.TypeNonNull (Syntax.NonNullTypeList (Syntax.ListType gt))) (encodeType cx g prefixes v0)
      Core.TypeMap v0 -> Eithers.map (\gt -> Syntax.TypeNonNull (Syntax.NonNullTypeList (Syntax.ListType gt))) (encodeType cx g prefixes (Core.mapTypeValues v0))
      Core.TypeLiteral v0 -> Eithers.map (\nt -> Syntax.TypeNonNull (Syntax.NonNullTypeNamed nt)) (encodeLiteralType cx v0)
      Core.TypePair _ -> Right (Syntax.TypeNonNull (Syntax.NonNullTypeNamed (Syntax.NamedType (encodeTypeName prefixes (Core.Name "hydra.util.Pair")))))
      Core.TypeEither _ -> Right (Syntax.TypeNonNull (Syntax.NonNullTypeNamed (Syntax.NamedType (encodeTypeName prefixes (Core.Name "hydra.util.Either")))))
      Core.TypeRecord _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous record type"))
      Core.TypeUnion _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous union type"))
      Core.TypeVariable v0 -> Right (Syntax.TypeNonNull (Syntax.NonNullTypeNamed (Syntax.NamedType (encodeTypeName prefixes v0))))
      Core.TypeWrap v0 -> encodeType cx g prefixes v0
      Core.TypeForall v0 -> encodeType cx g prefixes (Core.forallTypeBody v0)
      Core.TypeApplication v0 -> encodeType cx g prefixes (Core.applicationTypeFunction v0)
      Core.TypeFunction _ -> Right (Syntax.TypeNonNull (Syntax.NonNullTypeNamed (Syntax.NamedType (Syntax.Name "String"))))
      Core.TypeUnit -> Right (Syntax.TypeNonNull (Syntax.NonNullTypeNamed (Syntax.NamedType (Syntax.Name "Boolean"))))
      _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected GraphQL-compatible type, found: " (ShowCore.type_ typ))))

encodeTypeDefinition :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Packaging.TypeDefinition -> Either Errors.Error Syntax.TypeDefinition
encodeTypeDefinition cx g prefixes tdef =
    encodeNamedType cx g prefixes (Packaging.typeDefinitionName tdef) (Core.typeSchemeType (Packaging.typeDefinitionType tdef))

encodeTypeName :: M.Map Packaging.Namespace String -> Core.Name -> Syntax.Name
encodeTypeName prefixes name =

      let qualName = Names.qualifyName name
          local = Packaging.qualifiedNameLocal qualName
          mns = Packaging.qualifiedNameNamespace qualName
          prefix = Maybes.maybe "" (\ns_ -> Maybes.maybe "" (\p -> p) (Maps.lookup ns_ prefixes)) mns
      in (Syntax.Name (Strings.cat2 prefix (sanitize local)))

encodeUnionFieldType :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Core.FieldType -> Either Errors.Error Syntax.FieldDefinition
encodeUnionFieldType cx g prefixes ft =

      let innerType = Core.fieldTypeType ft
          isUnit = Predicates.isUnitType (Strip.deannotateType innerType)
          effectiveType = Logic.ifElse isUnit (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeBoolean)) (Core.TypeMaybe innerType)
      in (Eithers.bind (encodeType cx g prefixes effectiveType) (\gtype -> Eithers.bind (descriptionFromType cx g innerType) (\desc -> Right (Syntax.FieldDefinition {
        Syntax.fieldDefinitionDescription = desc,
        Syntax.fieldDefinitionName = (encodeFieldName (Core.fieldTypeName ft)),
        Syntax.fieldDefinitionArgumentsDefinition = Nothing,
        Syntax.fieldDefinitionType = gtype,
        Syntax.fieldDefinitionDirectives = Nothing}))))

moduleToGraphql :: Packaging.Module -> [Packaging.Definition] -> t0 -> Graph.Graph -> Either Errors.Error (M.Map String String)
moduleToGraphql mod defs cx g =

      let partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
          prefixes =
                  (\modNs -> \tdefs ->
                    let namespaces = Lists.nub (Maybes.cat (Lists.map (\td -> Names.namespaceOf (Packaging.typeDefinitionName td)) tdefs))
                    in (Maps.fromList (Lists.map (\ns_ -> (ns_, (Logic.ifElse (Equality.equal ns_ modNs) "" (Strings.cat2 (Formatting.sanitizeWithUnderscores Sets.empty (Packaging.unNamespace ns_)) "_")))) namespaces))) (Packaging.moduleNamespace mod) typeDefs
          filePath =
                  Names.namespaceToFilePath Util.CaseConventionCamel (Packaging.FileExtension "graphql") (Packaging.moduleNamespace mod)
      in (Eithers.bind (Eithers.mapList (\td -> encodeTypeDefinition cx g prefixes td) typeDefs) (\gtdefs -> Right (Maps.fromList (Lists.pure (filePath, (Serialization.printExpr (Serialization.parenthesize (Serde.exprDocument (Syntax.Document (Lists.map (\gtdef -> Syntax.DefinitionTypeSystem (Syntax.TypeSystemDefinitionOrExtensionDefinition (Syntax.TypeSystemDefinitionType gtdef))) gtdefs))))))))))

sanitize :: String -> String
sanitize s = Formatting.sanitizeWithUnderscores Language.graphqlReservedWords s
