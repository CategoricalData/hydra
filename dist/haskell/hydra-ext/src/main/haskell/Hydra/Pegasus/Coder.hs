-- Note: this is an automatically generated file. Do not edit.

-- | Pegasus PDL code generator: converts Hydra modules to PDL schema files

module Hydra.Pegasus.Coder where

import qualified Hydra.Analysis as Analysis
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Core as Core
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Environment as Environment
import qualified Hydra.Errors as Errors
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
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Pegasus.Pdl as Pdl
import qualified Hydra.Pegasus.Serde as Serde
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Strip as Strip
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Construct PDL schema files from type definitions, with topological sorting and cycle detection
constructModule :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Packaging.Module -> [Packaging.TypeDefinition] -> Either Errors.Error (M.Map String Pdl.SchemaFile)
constructModule cx g aliases mod typeDefs =

      let groups = Dependencies.topologicalSortTypeDefinitions typeDefs
      in (Maybes.cases (Lists.find (\grp -> Equality.gt (Lists.length grp) 1) groups) (
        let sortedDefs = Lists.concat groups
        in (Eithers.bind (Eithers.mapList (\typeDef -> typeToSchema cx g aliases mod typeDef) sortedDefs) (\schemas -> Right (Maps.fromList (Lists.map (toPair mod aliases) schemas))))) (\cycle -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "types form a cycle (unsupported in PDL): [" (Strings.cat2 (Strings.intercalate ", " (Lists.map (\td -> Core.unName (Packaging.typeDefinitionName td)) cycle)) "]"))))))

-- | Create PDL annotations from an optional doc string
doc :: Maybe String -> Pdl.Annotations
doc s =
    Pdl.Annotations {
      Pdl.annotationsDoc = s,
      Pdl.annotationsDeprecated = False}

encode :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Core.Type -> Either Errors.Error Pdl.Schema
encode cx g aliases t =
    case (Strip.deannotateType t) of
      Core.TypeRecord v0 -> Logic.ifElse (Lists.null v0) (encode cx g aliases (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Eithers.bind (encodeType cx g aliases t) (\res -> Eithers.either (\schema -> Right schema) (\_ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "type resolved to an unsupported nested named schema: " (Core_.type_ t))))) res))
      _ -> Eithers.bind (encodeType cx g aliases t) (\res -> Eithers.either (\schema -> Right schema) (\_ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "type resolved to an unsupported nested named schema: " (Core_.type_ t))))) res)

encodeEnumField :: t0 -> Graph.Graph -> Core.FieldType -> Either Errors.Error Pdl.EnumField
encodeEnumField cx g ft =

      let name = Core.fieldTypeName ft
          typ = Core.fieldTypeType ft
      in (Eithers.bind (getAnns cx g typ) (\anns -> Right (Pdl.EnumField {
        Pdl.enumFieldName = (Pdl.EnumFieldName (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake (Core.unName name))),
        Pdl.enumFieldAnnotations = anns})))

encodePossiblyOptionalType :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Core.Type -> Either Errors.Error (Pdl.Schema, Bool)
encodePossiblyOptionalType cx g aliases typ =
    case (Strip.deannotateType typ) of
      Core.TypeMaybe v0 -> Eithers.bind (encode cx g aliases v0) (\t -> Right (t, True))
      Core.TypeRecord _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeUnion _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeLiteral _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeList _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeMap _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeSet _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeVariable _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeWrap _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeEither _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypePair _ -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeVoid -> Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False))
      Core.TypeAnnotated v0 -> encodePossiblyOptionalType cx g aliases (Core.annotatedTypeBody v0)

encodeRecordField :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Core.FieldType -> Either Errors.Error Pdl.RecordField
encodeRecordField cx g aliases ft =

      let name = Core.fieldTypeName ft
          typ = Core.fieldTypeType ft
      in (Eithers.bind (getAnns cx g typ) (\anns -> Eithers.bind (encodePossiblyOptionalType cx g aliases typ) (\optResult ->
        let schema = Pairs.first optResult
            optional = Pairs.second optResult
        in (Right (Pdl.RecordField {
          Pdl.recordFieldName = (Pdl.FieldName (Core.unName name)),
          Pdl.recordFieldValue = schema,
          Pdl.recordFieldOptional = optional,
          Pdl.recordFieldDefault = Nothing,
          Pdl.recordFieldAnnotations = anns})))))

-- | Encode a Hydra type as either a PDL Schema (Left) or a PDL NamedSchemaType (Right)
encodeType :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Core.Type -> Either Errors.Error (Either Pdl.Schema Pdl.NamedSchemaType)
encodeType cx g aliases typ =
    case typ of
      Core.TypeAnnotated v0 -> encodeType cx g aliases (Core.annotatedTypeBody v0)
      Core.TypeEither v0 -> Eithers.bind (encode cx g aliases (Core.eitherTypeLeft v0)) (\leftSchema -> Eithers.bind (encode cx g aliases (Core.eitherTypeRight v0)) (\rightSchema ->
        let leftMember =
                Pdl.UnionMember {
                  Pdl.unionMemberAlias = (Just (Pdl.FieldName "left")),
                  Pdl.unionMemberValue = leftSchema,
                  Pdl.unionMemberAnnotations = noAnnotations}
            rightMember =
                    Pdl.UnionMember {
                      Pdl.unionMemberAlias = (Just (Pdl.FieldName "right")),
                      Pdl.unionMemberValue = rightSchema,
                      Pdl.unionMemberAnnotations = noAnnotations}
        in (Right (Left (Pdl.SchemaUnion (Pdl.UnionSchema [
          leftMember,
          rightMember]))))))
      Core.TypeList v0 -> Eithers.bind (encode cx g aliases v0) (\inner -> Right (Left (Pdl.SchemaArray inner)))
      Core.TypeLiteral v0 -> case v0 of
        Core.LiteralTypeBinary -> Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeBytes))
        Core.LiteralTypeBoolean -> Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeBoolean))
        Core.LiteralTypeFloat v1 -> case v1 of
          Core.FloatTypeFloat32 -> Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeFloat))
          Core.FloatTypeFloat64 -> Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeDouble))
          _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected " (Strings.cat2 "float32 or float64" (Strings.cat2 ", found: " (Core_.type_ typ))))))
        Core.LiteralTypeInteger v1 -> case v1 of
          Core.IntegerTypeInt32 -> Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeInt))
          Core.IntegerTypeInt64 -> Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeLong))
          _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected " (Strings.cat2 "int32 or int64" (Strings.cat2 ", found: " (Core_.type_ typ))))))
        Core.LiteralTypeString -> Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeString))
        _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected " (Strings.cat2 "PDL-supported literal type" (Strings.cat2 ", found: " (Core_.type_ typ))))))
      Core.TypeMap v0 -> Eithers.bind (encode cx g aliases (Core.mapTypeValues v0)) (\inner -> Right (Left (Pdl.SchemaMap inner)))
      Core.TypePair v0 -> Eithers.bind (encode cx g aliases (Core.pairTypeFirst v0)) (\firstSchema -> Eithers.bind (encode cx g aliases (Core.pairTypeSecond v0)) (\secondSchema ->
        let firstField =
                Pdl.RecordField {
                  Pdl.recordFieldName = (Pdl.FieldName "first"),
                  Pdl.recordFieldValue = firstSchema,
                  Pdl.recordFieldOptional = False,
                  Pdl.recordFieldDefault = Nothing,
                  Pdl.recordFieldAnnotations = noAnnotations}
            secondField =
                    Pdl.RecordField {
                      Pdl.recordFieldName = (Pdl.FieldName "second"),
                      Pdl.recordFieldValue = secondSchema,
                      Pdl.recordFieldOptional = False,
                      Pdl.recordFieldDefault = Nothing,
                      Pdl.recordFieldAnnotations = noAnnotations}
        in (Right (Right (Pdl.NamedSchemaTypeRecord (Pdl.RecordSchema {
          Pdl.recordSchemaFields = [
            firstField,
            secondField],
          Pdl.recordSchemaIncludes = []}))))))
      Core.TypeSet v0 -> Eithers.bind (encode cx g aliases v0) (\inner -> Right (Left (Pdl.SchemaArray inner)))
      Core.TypeVariable v0 -> Right (Left (Pdl.SchemaNamed (pdlNameForElement aliases True v0)))
      Core.TypeWrap v0 -> encodeType cx g aliases v0
      Core.TypeMaybe _ -> Left (Errors.ErrorOther (Errors.OtherError "optionals unexpected at top level"))
      Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (encodeRecordField cx g aliases) v0) (\rfields -> Right (Right (Pdl.NamedSchemaTypeRecord (Pdl.RecordSchema {
        Pdl.recordSchemaFields = rfields,
        Pdl.recordSchemaIncludes = []}))))
      Core.TypeUnion v0 -> Logic.ifElse (Lists.foldl (\b -> \t -> Logic.and b (Equality.equal (Strip.deannotateType t) Core.TypeUnit)) True (Lists.map (\f -> Core.fieldTypeType f) v0)) (Eithers.bind (Eithers.mapList (encodeEnumField cx g) v0) (\fs -> Right (Right (Pdl.NamedSchemaTypeEnum (Pdl.EnumSchema {
        Pdl.enumSchemaFields = fs}))))) (Eithers.bind (Eithers.mapList (encodeUnionField cx g aliases) v0) (\members -> Right (Left (Pdl.SchemaUnion (Pdl.UnionSchema members)))))
      _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Expected " (Strings.cat2 "PDL-supported type" (Strings.cat2 ", found: " (Core_.type_ typ))))))

encodeUnionField :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> Core.FieldType -> Either Errors.Error Pdl.UnionMember
encodeUnionField cx g aliases ft =

      let name = Core.fieldTypeName ft
          typ = Core.fieldTypeType ft
      in (Eithers.bind (getAnns cx g typ) (\anns -> Eithers.bind (encodePossiblyOptionalType cx g aliases typ) (\optResult ->
        let s = Pairs.first optResult
            optional = Pairs.second optResult
            schema =
                    Logic.ifElse optional (Pdl.SchemaUnion (Pdl.UnionSchema (Lists.map (\ms -> simpleUnionMember ms) [
                      Pdl.SchemaNull,
                      s]))) s
        in (Right (Pdl.UnionMember {
          Pdl.unionMemberAlias = (Just (Pdl.FieldName (Core.unName name))),
          Pdl.unionMemberValue = schema,
          Pdl.unionMemberAnnotations = anns})))))

getAnns :: t0 -> Graph.Graph -> Core.Type -> Either Errors.Error Pdl.Annotations
getAnns cx g typ = Eithers.bind (Annotations.getTypeDescription cx g typ) (\r -> Right (doc r))

-- | Compute import aliases for a module's dependencies
importAliasesForModule :: t0 -> Graph.Graph -> Packaging.Module -> Either Errors.Error (M.Map Packaging.Namespace String)
importAliasesForModule cx g mod =
    Eithers.bind (Analysis.moduleDependencyNamespaces cx g False True True False mod) (\nss -> Right (Maps.fromList (Lists.map (\ns_ -> (ns_, (slashesToDots (Packaging.unNamespace ns_)))) (Sets.toList nss))))

-- | Convert a Hydra module to a map of file paths to PDL schema strings
moduleToPdl :: Packaging.Module -> [Packaging.Definition] -> t0 -> Graph.Graph -> Either Errors.Error (M.Map String String)
moduleToPdl mod defs cx g =
    Eithers.bind (moduleToPegasusSchemas cx g mod defs) (\files -> Right (Maps.fromList (Lists.map (\pair -> (Pairs.first pair, (Serialization.printExpr (Serialization.parenthesize (Serde.exprSchemaFile (Pairs.second pair)))))) (Maps.toList files))))

-- | Convert a Hydra module and its definitions to PDL schema files
moduleToPegasusSchemas :: t0 -> Graph.Graph -> Packaging.Module -> [Packaging.Definition] -> Either Errors.Error (M.Map String Pdl.SchemaFile)
moduleToPegasusSchemas cx g mod defs =

      let partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
      in (Eithers.bind (importAliasesForModule cx g mod) (\aliases -> constructModule cx g aliases mod typeDefs))

-- | Empty PDL annotations
noAnnotations :: Pdl.Annotations
noAnnotations =
    Pdl.Annotations {
      Pdl.annotationsDoc = Nothing,
      Pdl.annotationsDeprecated = False}

-- | Convert a Hydra element name to a PDL qualified name
pdlNameForElement :: M.Map Packaging.Namespace String -> Bool -> Core.Name -> Pdl.QualifiedName
pdlNameForElement aliases withNs name =

      let qn = Names.qualifyName name
          ns_ = Packaging.qualifiedNameNamespace qn
          local = Packaging.qualifiedNameLocal qn
          alias = Maybes.bind ns_ (\n -> Maps.lookup n aliases)
      in Pdl.QualifiedName {
        Pdl.qualifiedNameName = (Pdl.Name local),
        Pdl.qualifiedNameNamespace = (Logic.ifElse withNs (Maybes.map (\a -> Pdl.Namespace a) alias) Nothing)}

-- | Convert a module's namespace to a PDL namespace
pdlNameForModule :: Packaging.Module -> Pdl.Namespace
pdlNameForModule mod = Pdl.Namespace (slashesToDots (Packaging.unNamespace (Packaging.moduleNamespace mod)))

-- | Create a simple union member without an alias
simpleUnionMember :: Pdl.Schema -> Pdl.UnionMember
simpleUnionMember schema =
    Pdl.UnionMember {
      Pdl.unionMemberAlias = Nothing,
      Pdl.unionMemberValue = schema,
      Pdl.unionMemberAnnotations = noAnnotations}

-- | Replace all forward slashes with dots in a string
slashesToDots :: String -> String
slashesToDots s = Strings.intercalate "." (Strings.splitOn "/" s)

toPair :: Packaging.Module -> t0 -> (Pdl.NamedSchema, [Pdl.QualifiedName]) -> (String, Pdl.SchemaFile)
toPair mod aliases schemaPair =

      let schema = Pairs.first schemaPair
          imports = Pairs.second schemaPair
          ns_ = pdlNameForModule mod
          local = Pdl.unName (Pdl.qualifiedNameName (Pdl.namedSchemaQualifiedName schema))
          path =
                  Names.namespaceToFilePath Util.CaseConventionCamel (Packaging.FileExtension "pdl") (Packaging.Namespace (Strings.cat2 (Packaging.unNamespace (Packaging.moduleNamespace mod)) (Strings.cat2 "/" local)))
      in (path, Pdl.SchemaFile {
        Pdl.schemaFileNamespace = ns_,
        Pdl.schemaFilePackage = Nothing,
        Pdl.schemaFileImports = imports,
        Pdl.schemaFileSchemas = [
          schema]})

typeToSchema :: t0 -> Graph.Graph -> M.Map Packaging.Namespace String -> t1 -> Packaging.TypeDefinition -> Either Errors.Error (Pdl.NamedSchema, [t2])
typeToSchema cx g aliases mod typeDef =

      let typ = Core.typeSchemeType (Packaging.typeDefinitionType typeDef)
      in (Eithers.bind (encodeType cx g aliases typ) (\res ->
        let ptype = Eithers.either (\schema -> Pdl.NamedSchemaTypeTyperef schema) (\t -> t) res
        in (Eithers.bind (Annotations.getTypeDescription cx g typ) (\descr ->
          let anns = doc descr
              qname = pdlNameForElement aliases False (Packaging.typeDefinitionName typeDef)
          in (Right (Pdl.NamedSchema {
            Pdl.namedSchemaQualifiedName = qname,
            Pdl.namedSchemaType = ptype,
            Pdl.namedSchemaAnnotations = anns}, []))))))
