-- Note: this is an automatically generated file. Do not edit.

-- | Pegasus PDL code generator: converts Hydra modules to PDL schema files

module Hydra.Ext.Pegasus.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Pegasus.Pdl as Pdl
import qualified Hydra.Ext.Pegasus.Serde as Serde
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

-- | Convert a Hydra module to a map of file paths to PDL schema strings
moduleToPdl :: (Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (M.Map String String))
moduleToPdl mod defs cx g = (Eithers.bind (moduleToPegasusSchemas cx g mod defs) (\files -> Right (Maps.fromList (Lists.map (\pair -> (Pairs.first pair, (Serialization.printExpr (Serialization.parenthesize (Serde.exprSchemaFile (Pairs.second pair)))))) (Maps.toList files)))))

-- | Construct PDL schema files from type definitions, with topological sorting and cycle detection
constructModule :: (Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Module.Module -> [Module.TypeDefinition] -> Either (Context.InContext Error.Error) (M.Map String Pdl.SchemaFile))
constructModule cx g aliases mod typeDefs =  
  let groups = (Schemas.topologicalSortTypeDefinitions typeDefs)
  in (Maybes.cases (Lists.find (\grp -> Equality.gt (Lists.length grp) 1) groups) ( 
    let sortedDefs = (Lists.concat groups)
    in (Eithers.bind (Eithers.mapList (\typeDef -> typeToSchema cx g aliases mod typeDef) sortedDefs) (\schemas -> Right (Maps.fromList (Lists.map (toPair mod aliases) schemas))))) (\cycle -> Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "types form a cycle (unsupported in PDL): [" (Strings.cat2 (Strings.intercalate ", " (Lists.map (\td -> Core.unName (Module.typeDefinitionName td)) cycle)) "]")))),
    Context.inContextContext = cx})))

typeToSchema :: (Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> t0 -> Module.TypeDefinition -> Either (Context.InContext Error.Error) (Pdl.NamedSchema, [t1]))
typeToSchema cx g aliases mod typeDef =  
  let typ = (Module.typeDefinitionType typeDef)
  in (Eithers.bind (encodeType cx g aliases typ) (\res ->  
    let ptype = (Eithers.either (\schema -> Pdl.NamedSchemaTypeTyperef schema) (\t -> t) res)
    in (Eithers.bind (Annotations.getTypeDescription cx g typ) (\descr ->  
      let anns = (doc descr)
      in  
        let qname = (pdlNameForElement aliases False (Module.typeDefinitionName typeDef))
        in (Right (Pdl.NamedSchema {
          Pdl.namedSchemaQualifiedName = qname,
          Pdl.namedSchemaType = ptype,
          Pdl.namedSchemaAnnotations = anns}, []))))))

toPair :: (Module.Module -> t0 -> (Pdl.NamedSchema, [Pdl.QualifiedName]) -> (String, Pdl.SchemaFile))
toPair mod aliases schemaPair =  
  let schema = (Pairs.first schemaPair)
  in  
    let imports = (Pairs.second schemaPair)
    in  
      let ns_ = (pdlNameForModule mod)
      in  
        let local = (Pdl.unName (Pdl.qualifiedNameName (Pdl.namedSchemaQualifiedName schema)))
        in  
          let path = (Names.namespaceToFilePath Util.CaseConventionCamel (Module.FileExtension "pdl") (Module.Namespace (Strings.cat2 (Module.unNamespace (Module.moduleNamespace mod)) (Strings.cat2 "/" local))))
          in (path, Pdl.SchemaFile {
            Pdl.schemaFileNamespace = ns_,
            Pdl.schemaFilePackage = Nothing,
            Pdl.schemaFileImports = imports,
            Pdl.schemaFileSchemas = [
              schema]})

-- | Convert a Hydra module and its definitions to PDL schema files
moduleToPegasusSchemas :: (Context.Context -> Graph.Graph -> Module.Module -> [Module.Definition] -> Either (Context.InContext Error.Error) (M.Map String Pdl.SchemaFile))
moduleToPegasusSchemas cx g mod defs =  
  let partitioned = (Schemas.partitionDefinitions defs)
  in  
    let typeDefs = (Pairs.first partitioned)
    in (Eithers.bind (importAliasesForModule cx g mod) (\aliases -> constructModule cx g aliases mod typeDefs))

-- | Create PDL annotations from an optional doc string
doc :: (Maybe String -> Pdl.Annotations)
doc s = Pdl.Annotations {
  Pdl.annotationsDoc = s,
  Pdl.annotationsDeprecated = False}

-- | Encode a Hydra type as either a PDL Schema (Left) or a PDL NamedSchemaType (Right)
encodeType :: (Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Core.Type -> Either (Context.InContext Error.Error) (Either Pdl.Schema Pdl.NamedSchemaType))
encodeType cx g aliases typ = ((\x -> case x of
  Core.TypeAnnotated v0 -> (encodeType cx g aliases (Core.annotatedTypeBody v0))
  Core.TypeEither v0 -> (Eithers.bind (encode cx g aliases (Core.eitherTypeLeft v0)) (\leftSchema -> Eithers.bind (encode cx g aliases (Core.eitherTypeRight v0)) (\rightSchema ->  
    let leftMember = Pdl.UnionMember {
            Pdl.unionMemberAlias = (Just (Pdl.FieldName "left")),
            Pdl.unionMemberValue = leftSchema,
            Pdl.unionMemberAnnotations = noAnnotations}
    in  
      let rightMember = Pdl.UnionMember {
              Pdl.unionMemberAlias = (Just (Pdl.FieldName "right")),
              Pdl.unionMemberValue = rightSchema,
              Pdl.unionMemberAnnotations = noAnnotations}
      in (Right (Left (Pdl.SchemaUnion (Pdl.UnionSchema [
        leftMember,
        rightMember])))))))
  Core.TypeList v0 -> (Eithers.bind (encode cx g aliases v0) (\inner -> Right (Left (Pdl.SchemaArray inner))))
  Core.TypeLiteral v0 -> ((\x -> case x of
    Core.LiteralTypeBinary -> (Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeBytes)))
    Core.LiteralTypeBoolean -> (Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeBoolean)))
    Core.LiteralTypeFloat v1 -> ((\x -> case x of
      Core.FloatTypeFloat32 -> (Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeFloat)))
      Core.FloatTypeFloat64 -> (Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeDouble)))
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected " (Strings.cat2 "float32 or float64" (Strings.cat2 ", found: " (Core_.type_ typ)))))),
        Context.inContextContext = cx}))) v1)
    Core.LiteralTypeInteger v1 -> ((\x -> case x of
      Core.IntegerTypeInt32 -> (Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeInt)))
      Core.IntegerTypeInt64 -> (Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeLong)))
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected " (Strings.cat2 "int32 or int64" (Strings.cat2 ", found: " (Core_.type_ typ)))))),
        Context.inContextContext = cx}))) v1)
    Core.LiteralTypeString -> (Right (Left (Pdl.SchemaPrimitive Pdl.PrimitiveTypeString)))
    _ -> (Left (Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected " (Strings.cat2 "PDL-supported literal type" (Strings.cat2 ", found: " (Core_.type_ typ)))))),
      Context.inContextContext = cx}))) v0)
  Core.TypeMap v0 -> (Eithers.bind (encode cx g aliases (Core.mapTypeValues v0)) (\inner -> Right (Left (Pdl.SchemaMap inner))))
  Core.TypePair v0 -> (Eithers.bind (encode cx g aliases (Core.pairTypeFirst v0)) (\firstSchema -> Eithers.bind (encode cx g aliases (Core.pairTypeSecond v0)) (\secondSchema ->  
    let firstField = Pdl.RecordField {
            Pdl.recordFieldName = (Pdl.FieldName "first"),
            Pdl.recordFieldValue = firstSchema,
            Pdl.recordFieldOptional = False,
            Pdl.recordFieldDefault = Nothing,
            Pdl.recordFieldAnnotations = noAnnotations}
    in  
      let secondField = Pdl.RecordField {
              Pdl.recordFieldName = (Pdl.FieldName "second"),
              Pdl.recordFieldValue = secondSchema,
              Pdl.recordFieldOptional = False,
              Pdl.recordFieldDefault = Nothing,
              Pdl.recordFieldAnnotations = noAnnotations}
      in (Right (Right (Pdl.NamedSchemaTypeRecord (Pdl.RecordSchema {
        Pdl.recordSchemaFields = [
          firstField,
          secondField],
        Pdl.recordSchemaIncludes = []})))))))
  Core.TypeSet v0 -> (Eithers.bind (encode cx g aliases v0) (\inner -> Right (Left (Pdl.SchemaArray inner))))
  Core.TypeVariable v0 -> (Right (Left (Pdl.SchemaNamed (pdlNameForElement aliases True v0))))
  Core.TypeWrap v0 -> (encodeType cx g aliases v0)
  Core.TypeMaybe _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError "optionals unexpected at top level")),
    Context.inContextContext = cx}))
  Core.TypeRecord v0 -> (Eithers.bind (Eithers.mapList (encodeRecordField cx g aliases) v0) (\rfields -> Right (Right (Pdl.NamedSchemaTypeRecord (Pdl.RecordSchema {
    Pdl.recordSchemaFields = rfields,
    Pdl.recordSchemaIncludes = []})))))
  Core.TypeUnion v0 -> (Logic.ifElse (Lists.foldl (\b -> \t -> Logic.and b (Equality.equal (Rewriting.deannotateType t) Core.TypeUnit)) True (Lists.map (\f -> Core.fieldTypeType f) v0)) (Eithers.bind (Eithers.mapList (encodeEnumField cx g) v0) (\fs -> Right (Right (Pdl.NamedSchemaTypeEnum (Pdl.EnumSchema {
    Pdl.enumSchemaFields = fs}))))) (Eithers.bind (Eithers.mapList (encodeUnionField cx g aliases) v0) (\members -> Right (Left (Pdl.SchemaUnion (Pdl.UnionSchema members))))))
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Expected " (Strings.cat2 "PDL-supported type" (Strings.cat2 ", found: " (Core_.type_ typ)))))),
    Context.inContextContext = cx}))) typ)

encode :: (Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Core.Type -> Either (Context.InContext Error.Error) Pdl.Schema)
encode cx g aliases t = ((\x -> case x of
  Core.TypeRecord v0 -> (Logic.ifElse (Lists.null v0) (encode cx g aliases (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Eithers.bind (encodeType cx g aliases t) (\res -> Eithers.either (\schema -> Right schema) (\_ -> Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "type resolved to an unsupported nested named schema: " (Core_.type_ t)))),
    Context.inContextContext = cx})) res)))
  _ -> (Eithers.bind (encodeType cx g aliases t) (\res -> Eithers.either (\schema -> Right schema) (\_ -> Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "type resolved to an unsupported nested named schema: " (Core_.type_ t)))),
    Context.inContextContext = cx})) res))) (Rewriting.deannotateType t))

encodeRecordField :: (Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Core.FieldType -> Either (Context.InContext Error.Error) Pdl.RecordField)
encodeRecordField cx g aliases ft =  
  let name = (Core.fieldTypeName ft)
  in  
    let typ = (Core.fieldTypeType ft)
    in (Eithers.bind (getAnns cx g typ) (\anns -> Eithers.bind (encodePossiblyOptionalType cx g aliases typ) (\optResult ->  
      let schema = (Pairs.first optResult)
      in  
        let optional = (Pairs.second optResult)
        in (Right (Pdl.RecordField {
          Pdl.recordFieldName = (Pdl.FieldName (Core.unName name)),
          Pdl.recordFieldValue = schema,
          Pdl.recordFieldOptional = optional,
          Pdl.recordFieldDefault = Nothing,
          Pdl.recordFieldAnnotations = anns})))))

encodeUnionField :: (Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Core.FieldType -> Either (Context.InContext Error.Error) Pdl.UnionMember)
encodeUnionField cx g aliases ft =  
  let name = (Core.fieldTypeName ft)
  in  
    let typ = (Core.fieldTypeType ft)
    in (Eithers.bind (getAnns cx g typ) (\anns -> Eithers.bind (encodePossiblyOptionalType cx g aliases typ) (\optResult ->  
      let s = (Pairs.first optResult)
      in  
        let optional = (Pairs.second optResult)
        in  
          let schema = (Logic.ifElse optional (Pdl.SchemaUnion (Pdl.UnionSchema (Lists.map (\ms -> simpleUnionMember ms) [
                  Pdl.SchemaNull,
                  s]))) s)
          in (Right (Pdl.UnionMember {
            Pdl.unionMemberAlias = (Just (Pdl.FieldName (Core.unName name))),
            Pdl.unionMemberValue = schema,
            Pdl.unionMemberAnnotations = anns})))))

encodeEnumField :: (Context.Context -> Graph.Graph -> Core.FieldType -> Either (Context.InContext Error.Error) Pdl.EnumField)
encodeEnumField cx g ft =  
  let name = (Core.fieldTypeName ft)
  in  
    let typ = (Core.fieldTypeType ft)
    in (Eithers.bind (getAnns cx g typ) (\anns -> Right (Pdl.EnumField {
      Pdl.enumFieldName = (Pdl.EnumFieldName (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake (Core.unName name))),
      Pdl.enumFieldAnnotations = anns})))

encodePossiblyOptionalType :: (Context.Context -> Graph.Graph -> M.Map Module.Namespace String -> Core.Type -> Either (Context.InContext Error.Error) (Pdl.Schema, Bool))
encodePossiblyOptionalType cx g aliases typ = ((\x -> case x of
  Core.TypeMaybe v0 -> (Eithers.bind (encode cx g aliases v0) (\t -> Right (t, True)))
  Core.TypeRecord _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeUnion _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeLiteral _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeList _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeMap _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeSet _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeVariable _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeWrap _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeEither _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypePair _ -> (Eithers.bind (encode cx g aliases typ) (\t -> Right (t, False)))
  Core.TypeAnnotated v0 -> (encodePossiblyOptionalType cx g aliases (Core.annotatedTypeBody v0))) (Rewriting.deannotateType typ))

getAnns :: (Context.Context -> Graph.Graph -> Core.Type -> Either (Context.InContext Error.Error) Pdl.Annotations)
getAnns cx g typ = (Eithers.bind (Annotations.getTypeDescription cx g typ) (\r -> Right (doc r)))

-- | Compute import aliases for a module's dependencies
importAliasesForModule :: (Context.Context -> Graph.Graph -> Module.Module -> Either (Context.InContext Error.Error) (M.Map Module.Namespace String))
importAliasesForModule cx g mod = (Eithers.bind (Schemas.moduleDependencyNamespaces cx g False True True False mod) (\nss -> Right (Maps.fromList (Lists.map (\ns_ -> (ns_, (slashesToDots (Module.unNamespace ns_)))) (Sets.toList nss)))))

-- | Empty PDL annotations
noAnnotations :: Pdl.Annotations
noAnnotations = Pdl.Annotations {
  Pdl.annotationsDoc = Nothing,
  Pdl.annotationsDeprecated = False}

-- | Convert a Hydra element name to a PDL qualified name
pdlNameForElement :: (M.Map Module.Namespace String -> Bool -> Core.Name -> Pdl.QualifiedName)
pdlNameForElement aliases withNs name =  
  let qn = (Names.qualifyName name)
  in  
    let ns_ = (Module.qualifiedNameNamespace qn)
    in  
      let local = (Module.qualifiedNameLocal qn)
      in  
        let alias = (Maybes.bind ns_ (\n -> Maps.lookup n aliases))
        in Pdl.QualifiedName {
          Pdl.qualifiedNameName = (Pdl.Name local),
          Pdl.qualifiedNameNamespace = (Logic.ifElse withNs (Maybes.map (\a -> Pdl.Namespace a) alias) Nothing)}

-- | Convert a module's namespace to a PDL namespace
pdlNameForModule :: (Module.Module -> Pdl.Namespace)
pdlNameForModule mod = (Pdl.Namespace (slashesToDots (Module.unNamespace (Module.moduleNamespace mod))))

-- | Create a simple union member without an alias
simpleUnionMember :: (Pdl.Schema -> Pdl.UnionMember)
simpleUnionMember schema = Pdl.UnionMember {
  Pdl.unionMemberAlias = Nothing,
  Pdl.unionMemberValue = schema,
  Pdl.unionMemberAnnotations = noAnnotations}

-- | Replace all forward slashes with dots in a string
slashesToDots :: (String -> String)
slashesToDots s = (Strings.intercalate "." (Strings.splitOn "/" s))
