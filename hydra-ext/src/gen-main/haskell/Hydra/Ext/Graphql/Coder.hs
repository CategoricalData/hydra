-- Note: this is an automatically generated file. Do not edit.

-- | GraphQL code generator: converts Hydra modules to GraphQL schema definitions

module Hydra.Ext.Graphql.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Graphql.Language as Language
import qualified Hydra.Ext.Graphql.Serde as Serde
import qualified Hydra.Ext.Org.Graphql.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

moduleToGraphql :: Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (M.Map String String)
moduleToGraphql mod defs cx g =
     
      let partitioned = Schemas.partitionDefinitions defs 
          typeDefs = Pairs.first partitioned
          prefixes =
                  (\modNs -> \tdefs ->  
                    let namespaces = Lists.nub (Maybes.cat (Lists.map (\td -> Names.namespaceOf (Module.typeDefinitionName td)) tdefs))
                    in (Maps.fromList (Lists.map (\ns_ -> (ns_, (Logic.ifElse (Equality.equal ns_ modNs) "" (Strings.cat2 (Formatting.sanitizeWithUnderscores Sets.empty (Module.unNamespace ns_)) "_")))) namespaces))) (Module.moduleNamespace mod) typeDefs
          filePath = Names.namespaceToFilePath Util.CaseConventionCamel (Module.FileExtension "graphql") (Module.moduleNamespace mod)
      in (Eithers.bind (Eithers.mapList (\td -> encodeTypeDefinition cx g prefixes td) typeDefs) (\gtdefs -> Right (Maps.fromList (Lists.pure (filePath, (Serialization.printExpr (Serialization.parenthesize (Serde.exprDocument (Syntax.Document (Lists.map (\gtdef -> Syntax.DefinitionTypeSystem (Syntax.TypeSystemDefinitionOrExtensionDefinition (Syntax.TypeSystemDefinitionType gtdef))) gtdefs))))))))))

encodeTypeDefinition :: Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Module.TypeDefinition -> Either (Context.InContext Error.Error) Syntax.TypeDefinition
encodeTypeDefinition cx g prefixes tdef =
    encodeNamedType cx g prefixes (Module.typeDefinitionName tdef) (Module.typeDefinitionType tdef)

descriptionFromType :: Context.Context -> Graph.Graph -> Core.Type -> Either (Context.InContext Error.Error) (Maybe Syntax.Description)
descriptionFromType cx g typ =
    Eithers.map (\mval -> Maybes.map (\s -> Syntax.Description (Syntax.StringValue s)) mval) (Annotations.getTypeDescription cx g typ)

encodeEnumFieldType :: Context.Context -> Graph.Graph -> Core.FieldType -> Either (Context.InContext Error.Error) Syntax.EnumValueDefinition
encodeEnumFieldType cx g ft =
    Eithers.bind (descriptionFromType cx g (Core.fieldTypeType ft)) (\desc -> Right (Syntax.EnumValueDefinition {
      Syntax.enumValueDefinitionDescription = desc,
      Syntax.enumValueDefinitionEnumValue = (encodeEnumFieldName (Core.fieldTypeName ft)),
      Syntax.enumValueDefinitionDirectives = Nothing}))

encodeEnumFieldName :: Core.Name -> Syntax.EnumValue
encodeEnumFieldName name = Syntax.EnumValue (Syntax.Name (sanitize (Core.unName name)))

encodeFieldName :: Core.Name -> Syntax.Name
encodeFieldName name = Syntax.Name (sanitize (Core.unName name))

encodeFieldType :: Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Core.FieldType -> Either (Context.InContext Error.Error) Syntax.FieldDefinition
encodeFieldType cx g prefixes ft =
    Eithers.bind (encodeType cx g prefixes (Core.fieldTypeType ft)) (\gtype -> Eithers.bind (descriptionFromType cx g (Core.fieldTypeType ft)) (\desc -> Right (Syntax.FieldDefinition {
      Syntax.fieldDefinitionDescription = desc,
      Syntax.fieldDefinitionName = (encodeFieldName (Core.fieldTypeName ft)),
      Syntax.fieldDefinitionArgumentsDefinition = Nothing,
      Syntax.fieldDefinitionType = gtype,
      Syntax.fieldDefinitionDirectives = Nothing})))

encodeLiteralType :: Context.Context -> Core.LiteralType -> Either (Context.InContext Error.Error) Syntax.NamedType
encodeLiteralType cx lt =
    case lt of
      Core.LiteralTypeBoolean -> Right (Syntax.NamedType (Syntax.Name "Boolean"))
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeFloat64 -> Right (Syntax.NamedType (Syntax.Name "Float"))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected 64-bit float type, found: " (Core_.floatType v0)))),
          Context.inContextContext = cx})
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeInt32 -> Right (Syntax.NamedType (Syntax.Name "Int"))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected 32-bit signed integer type, found: " (Core_.integerType v0)))),
          Context.inContextContext = cx})
      Core.LiteralTypeString -> Right (Syntax.NamedType (Syntax.Name "String"))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected GraphQL-compatible literal type, found: " (Core_.literalType lt)))),
        Context.inContextContext = cx})

encodeNamedType :: Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Core.Name -> Core.Type -> Either (Context.InContext Error.Error) Syntax.TypeDefinition
encodeNamedType cx g prefixes name typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (\f -> encodeFieldType cx g prefixes f) v0) (\gfields -> Eithers.bind (descriptionFromType cx g typ) (\desc -> Right (Syntax.TypeDefinitionObject (Syntax.ObjectTypeDefinition {
        Syntax.objectTypeDefinitionDescription = desc,
        Syntax.objectTypeDefinitionName = (encodeTypeName prefixes name),
        Syntax.objectTypeDefinitionImplementsInterfaces = Nothing,
        Syntax.objectTypeDefinitionDirectives = Nothing,
        Syntax.objectTypeDefinitionFieldsDefinition = (Just (Syntax.FieldsDefinition gfields))}))))
      Core.TypeUnion v0 -> Eithers.bind (Eithers.mapList (\f -> encodeEnumFieldType cx g f) v0) (\values -> Eithers.bind (descriptionFromType cx g typ) (\desc -> Right (Syntax.TypeDefinitionEnum (Syntax.EnumTypeDefinition {
        Syntax.enumTypeDefinitionDescription = desc,
        Syntax.enumTypeDefinitionName = (encodeTypeName prefixes name),
        Syntax.enumTypeDefinitionDirectives = Nothing,
        Syntax.enumTypeDefinitionEnumValuesDefinition = (Just (Syntax.EnumValuesDefinition values))}))))
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
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = (Core.TypeList (Core.mapTypeValues v0))}])
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
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected record or union type, found: " (Core_.type_ typ)))),
        Context.inContextContext = cx})

encodeType :: Context.Context -> t0 -> M.Map Module.Namespace String -> Core.Type -> Either (Context.InContext Error.Error) Syntax.Type
encodeType cx g prefixes typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeMaybe v0 -> case (Rewriting.deannotateType v0) of
        Core.TypeList v1 -> Eithers.map (\gt -> Syntax.TypeList (Syntax.ListType gt)) (encodeType cx g prefixes v1)
        Core.TypeSet v1 -> Eithers.map (\gt -> Syntax.TypeList (Syntax.ListType gt)) (encodeType cx g prefixes v1)
        Core.TypeMap v1 -> Eithers.map (\gt -> Syntax.TypeList (Syntax.ListType gt)) (encodeType cx g prefixes (Core.mapTypeValues v1))
        Core.TypeLiteral v1 -> Eithers.map (\nt -> Syntax.TypeNamed nt) (encodeLiteralType cx v1)
        Core.TypeRecord _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous record type")),
          Context.inContextContext = cx})
        Core.TypeUnion _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous union type")),
          Context.inContextContext = cx})
        Core.TypeWrap _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous wrap type")),
          Context.inContextContext = cx})
        Core.TypeVariable v1 -> Right (Syntax.TypeNamed (Syntax.NamedType (encodeTypeName prefixes v1)))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected GraphQL-compatible type, found: " (Core_.type_ v0)))),
          Context.inContextContext = cx})
      Core.TypeList v0 -> Eithers.map (\gt -> Syntax.TypeNonNull (Syntax.NonNullTypeList (Syntax.ListType gt))) (encodeType cx g prefixes v0)
      Core.TypeSet v0 -> Eithers.map (\gt -> Syntax.TypeNonNull (Syntax.NonNullTypeList (Syntax.ListType gt))) (encodeType cx g prefixes v0)
      Core.TypeMap v0 -> Eithers.map (\gt -> Syntax.TypeNonNull (Syntax.NonNullTypeList (Syntax.ListType gt))) (encodeType cx g prefixes (Core.mapTypeValues v0))
      Core.TypeLiteral v0 -> Eithers.map (\nt -> Syntax.TypeNonNull (Syntax.NonNullTypeNamed nt)) (encodeLiteralType cx v0)
      Core.TypeRecord _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous record type")),
        Context.inContextContext = cx})
      Core.TypeUnion _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous union type")),
        Context.inContextContext = cx})
      Core.TypeVariable v0 -> Right (Syntax.TypeNonNull (Syntax.NonNullTypeNamed (Syntax.NamedType (encodeTypeName prefixes v0))))
      Core.TypeWrap _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "unexpected anonymous wrap type")),
        Context.inContextContext = cx})
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected GraphQL-compatible type, found: " (Core_.type_ typ)))),
        Context.inContextContext = cx})

encodeTypeName :: M.Map Module.Namespace String -> Core.Name -> Syntax.Name
encodeTypeName prefixes name =
     
      let qualName = Names.qualifyName name 
          local = Module.qualifiedNameLocal qualName
          mns = Module.qualifiedNameNamespace qualName
          prefix = Maybes.maybe "" (\ns_ -> Maybes.maybe "" (\p -> p) (Maps.lookup ns_ prefixes)) mns
      in (Syntax.Name (Strings.cat2 prefix (sanitize local)))

sanitize :: String -> String
sanitize s = Formatting.sanitizeWithUnderscores Language.graphqlReservedWords s
