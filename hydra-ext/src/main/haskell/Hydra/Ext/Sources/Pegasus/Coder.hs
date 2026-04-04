module Hydra.Ext.Sources.Pegasus.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Analysis      as Analysis
import qualified Hydra.Sources.Kernel.Terms.Environment   as Environment
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Ext.Pegasus.Pdl as PDL
import qualified Hydra.Ext.Sources.Pegasus.Pdl as PdlSyntax
import qualified Hydra.Ext.Sources.Pegasus.Language as PegasusLanguageSource
import qualified Hydra.Ext.Sources.Pegasus.Serde as PegasusSerdeSource
import qualified Hydra.Sources.Kernel.Terms.Dependencies as Dependencies


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.ext.pegasus.coder"

module_ :: Module
module_ = Module ns elements
    [PegasusSerdeSource.ns, moduleNamespace PegasusLanguageSource.module_, Formatting.ns, Names.ns, Analysis.ns, Environment.ns, Sorting.ns, Strip.ns, Annotations.ns, Serialization.ns, ShowCore.ns]
    (PdlSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Pegasus PDL code generator: converts Hydra modules to PDL schema files"
  where
    elements = [
      toDefinition moduleToPdl,
      toDefinition constructModule,
      toDefinition typeToSchema,
      toDefinition toPair,
      toDefinition moduleToPegasusSchemas,
      toDefinition doc_,
      toDefinition encodeType_,
      toDefinition encode,
      toDefinition encodeRecordField,
      toDefinition encodeUnionField,
      toDefinition encodeEnumField,
      toDefinition encodePossiblyOptionalType,
      toDefinition getAnns,
      toDefinition importAliasesForModule,
      toDefinition noAnnotations_,
      toDefinition pdlNameForElement,
      toDefinition pdlNameForModule,
      toDefinition simpleUnionMember,
      toDefinition slashesToDots]


-- | err cx msg = Left (InContext (ErrorOther (OtherError msg)) cx)
err :: TTerm Context -> TTerm String -> TTerm (Either (InContext Error) a)
err cx msg = Ctx.failInContext (Error.errorOther $ Error.otherError msg) cx

-- | unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found
unexpectedE :: TTerm Context -> TTerm String -> TTerm String -> TTerm (Either (InContext Error) a)
unexpectedE cx expected found = err cx (Strings.cat2 (string "Expected ") (Strings.cat2 expected (Strings.cat2 (string ", found: ") found)))


moduleToPdl :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either (InContext Error) (M.Map FilePath String))
moduleToPdl = def "moduleToPdl" $
  doc "Convert a Hydra module to a map of file paths to PDL schema strings" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
    "files" <<~ (moduleToPegasusSchemas @@ var "cx" @@ var "g" @@ var "mod" @@ var "defs") $
    right (Maps.fromList (Lists.map
      (lambda "pair" $
        pair
          (Pairs.first (var "pair"))
          (Serialization.printExpr @@ (Serialization.parenthesize @@ (PegasusSerdeSource.exprSchemaFile @@ Pairs.second (var "pair")))))
      (Maps.toList (var "files"))))


constructModule :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> Module -> [TypeDefinition] -> Either (InContext Error) (M.Map FilePath PDL.SchemaFile))
constructModule = def "constructModule" $
  doc "Construct PDL schema files from type definitions, with topological sorting and cycle detection" $
  "cx" ~> "g" ~> "aliases" ~> "mod" ~> "typeDefs" ~>
    "groups" <~ (Dependencies.topologicalSortTypeDefinitions @@ var "typeDefs") $
    -- Check for cycles: if any group has more than one element, it's a cycle
    Maybes.cases (Lists.find (lambda "grp" $ Equality.gt (Lists.length (var "grp")) (int32 1)) (var "groups"))
      -- No cycle found: flatten and process
      ("sortedDefs" <~ Lists.concat (var "groups") $
       "schemas" <<~ (Eithers.mapList (lambda "typeDef" $ typeToSchema @@ var "cx" @@ var "g" @@ var "aliases" @@ var "mod" @@ var "typeDef") (var "sortedDefs")) $
       right (Maps.fromList (Lists.map (toPair @@ var "mod" @@ var "aliases") (var "schemas"))))
      -- Cycle found
      (lambda "cycle" $
        err (var "cx") (Strings.cat2 (string "types form a cycle (unsupported in PDL): [") (Strings.cat2 (Strings.intercalate (string ", ") (Lists.map (lambda "td" $ Core.unName (Packaging.typeDefinitionName (var "td"))) (var "cycle"))) (string "]"))))

typeToSchema :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> Module -> TypeDefinition -> Either (InContext Error) (PDL.NamedSchema, [PDL.QualifiedName]))
typeToSchema = def "typeToSchema" $
  "cx" ~> "g" ~> "aliases" ~> "mod" ~> "typeDef" ~>
    "typ" <~ (Core.typeSchemeType $ Packaging.typeDefinitionType (var "typeDef")) $
    "res" <<~ (encodeType_ @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
    "ptype" <~ (Eithers.either_
      (lambda "schema" $ inject PDL._NamedSchemaType PDL._NamedSchemaType_typeref (var "schema"))
      (lambda "t" $ var "t")
      (var "res")) $
    "descr" <<~ (Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "typ") $
    "anns" <~ (doc_ @@ var "descr") $
    "qname" <~ (pdlNameForElement @@ var "aliases" @@ false @@ Packaging.typeDefinitionName (var "typeDef")) $
    right (pair (record PDL._NamedSchema [
      PDL._NamedSchema_qualifiedName>>: var "qname",
      PDL._NamedSchema_type>>: var "ptype",
      PDL._NamedSchema_annotations>>: var "anns"])
      (list ([] :: [TTerm PDL.QualifiedName])))

toPair :: TTermDefinition (Module -> M.Map Namespace String -> (PDL.NamedSchema, [PDL.QualifiedName]) -> (FilePath, PDL.SchemaFile))
toPair = def "toPair" $
  "mod" ~> "aliases" ~> "schemaPair" ~>
    "schema" <~ Pairs.first (var "schemaPair") $
    "imports" <~ Pairs.second (var "schemaPair") $
    "ns_" <~ (pdlNameForModule @@ var "mod") $
    "local" <~ (unwrap PDL._Name @@ (project PDL._QualifiedName PDL._QualifiedName_name @@ (project PDL._NamedSchema PDL._NamedSchema_qualifiedName @@ var "schema"))) $
    "path" <~ (Names.namespaceToFilePath @@ Util.caseConventionCamel @@ wrap _FileExtension (string "pdl") @@ (wrap _Namespace (Strings.cat2 (unwrap _Namespace @@ Packaging.moduleNamespace (var "mod")) (Strings.cat2 (string "/") (var "local"))))) $
    pair (var "path") (record PDL._SchemaFile [
      PDL._SchemaFile_namespace>>: var "ns_",
      PDL._SchemaFile_package>>: nothing,
      PDL._SchemaFile_imports>>: var "imports",
      PDL._SchemaFile_schemas>>: list [var "schema"]])


moduleToPegasusSchemas :: TTermDefinition (Context -> Graph -> Module -> [Definition] -> Either (InContext Error) (M.Map FilePath PDL.SchemaFile))
moduleToPegasusSchemas = def "moduleToPegasusSchemas" $
  doc "Convert a Hydra module and its definitions to PDL schema files" $
  "cx" ~> "g" ~> "mod" ~> "defs" ~>
    "partitioned" <~ (Environment.partitionDefinitions @@ var "defs") $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "aliases" <<~ (importAliasesForModule @@ var "cx" @@ var "g" @@ var "mod") $
    constructModule @@ var "cx" @@ var "g" @@ var "aliases" @@ var "mod" @@ var "typeDefs"


doc_ :: TTermDefinition (Maybe String -> PDL.Annotations)
doc_ = def "doc" $
  doc "Create PDL annotations from an optional doc string" $
  "s" ~> record PDL._Annotations [
    PDL._Annotations_doc>>: var "s",
    PDL._Annotations_deprecated>>: false]


encodeType_ :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> Type -> Either (InContext Error) (Either PDL.Schema PDL.NamedSchemaType))
encodeType_ = def "encodeType" $
  doc "Encode a Hydra type as either a PDL Schema (Left) or a PDL NamedSchemaType (Right)" $
  "cx" ~> "g" ~> "aliases" ~> "typ" ~>
    cases _Type (var "typ")
      (Just $ unexpectedE (var "cx") (string "PDL-supported type") (ShowCore.type_ @@ var "typ")) [
      _Type_annotated>>: lambda "at" $
        encodeType_ @@ var "cx" @@ var "g" @@ var "aliases" @@ Core.annotatedTypeBody (var "at"),
      _Type_either>>: lambda "et" $
        "leftSchema" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ Core.eitherTypeLeft (var "et")) $
        "rightSchema" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ Core.eitherTypeRight (var "et")) $
        "leftMember" <~ record PDL._UnionMember [
          PDL._UnionMember_alias>>: just (wrap PDL._FieldName (string "left")),
          PDL._UnionMember_value>>: var "leftSchema",
          PDL._UnionMember_annotations>>: noAnnotations_] $
        "rightMember" <~ record PDL._UnionMember [
          PDL._UnionMember_alias>>: just (wrap PDL._FieldName (string "right")),
          PDL._UnionMember_value>>: var "rightSchema",
          PDL._UnionMember_annotations>>: noAnnotations_] $
        right (left (inject PDL._Schema PDL._Schema_union (wrap PDL._UnionSchema (list [var "leftMember", var "rightMember"])))),
      _Type_list>>: lambda "lt" $
        "inner" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "lt") $
        right (left (inject PDL._Schema PDL._Schema_array (var "inner"))),
      _Type_literal>>: lambda "lt" $
        cases _LiteralType (var "lt")
          (Just $ unexpectedE (var "cx") (string "PDL-supported literal type") (ShowCore.type_ @@ var "typ")) [
          _LiteralType_binary>>: constant $
            right (left (inject PDL._Schema PDL._Schema_primitive (inject PDL._PrimitiveType PDL._PrimitiveType_bytes (unit)))),
          _LiteralType_boolean>>: constant $
            right (left (inject PDL._Schema PDL._Schema_primitive (inject PDL._PrimitiveType PDL._PrimitiveType_boolean (unit)))),
          _LiteralType_float>>: lambda "ft" $
            cases _FloatType (var "ft")
              (Just $ unexpectedE (var "cx") (string "float32 or float64") (ShowCore.type_ @@ var "typ")) [
              _FloatType_float32>>: constant $
                right (left (inject PDL._Schema PDL._Schema_primitive (inject PDL._PrimitiveType PDL._PrimitiveType_float (unit)))),
              _FloatType_float64>>: constant $
                right (left (inject PDL._Schema PDL._Schema_primitive (inject PDL._PrimitiveType PDL._PrimitiveType_double (unit))))],
          _LiteralType_integer>>: lambda "it" $
            cases _IntegerType (var "it")
              (Just $ unexpectedE (var "cx") (string "int32 or int64") (ShowCore.type_ @@ var "typ")) [
              _IntegerType_int32>>: constant $
                right (left (inject PDL._Schema PDL._Schema_primitive (inject PDL._PrimitiveType PDL._PrimitiveType_int (unit)))),
              _IntegerType_int64>>: constant $
                right (left (inject PDL._Schema PDL._Schema_primitive (inject PDL._PrimitiveType PDL._PrimitiveType_long (unit))))],
          _LiteralType_string>>: constant $
            right (left (inject PDL._Schema PDL._Schema_primitive (inject PDL._PrimitiveType PDL._PrimitiveType_string (unit))))],
      _Type_map>>: lambda "mt" $
        -- note: we simply assume string as a key type
        "inner" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ Core.mapTypeValues (var "mt")) $
        right (left (inject PDL._Schema PDL._Schema_map (var "inner"))),
      _Type_pair>>: lambda "pt" $
        "firstSchema" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ Core.pairTypeFirst (var "pt")) $
        "secondSchema" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ Core.pairTypeSecond (var "pt")) $
        "firstField" <~ record PDL._RecordField [
          PDL._RecordField_name>>: wrap PDL._FieldName (string "first"),
          PDL._RecordField_value>>: var "firstSchema",
          PDL._RecordField_optional>>: false,
          PDL._RecordField_default>>: nothing,
          PDL._RecordField_annotations>>: noAnnotations_] $
        "secondField" <~ record PDL._RecordField [
          PDL._RecordField_name>>: wrap PDL._FieldName (string "second"),
          PDL._RecordField_value>>: var "secondSchema",
          PDL._RecordField_optional>>: false,
          PDL._RecordField_default>>: nothing,
          PDL._RecordField_annotations>>: noAnnotations_] $
        right (right (inject PDL._NamedSchemaType PDL._NamedSchemaType_record (record PDL._RecordSchema [
          PDL._RecordSchema_fields>>: list [var "firstField", var "secondField"],
          PDL._RecordSchema_includes>>: list ([] :: [TTerm PDL.NamedSchema])]))),
      _Type_set>>: lambda "st" $
        -- Encode Set as array (PDL has no native set type)
        "inner" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "st") $
        right (left (inject PDL._Schema PDL._Schema_array (var "inner"))),
      _Type_variable>>: lambda "name" $
        right (left (inject PDL._Schema PDL._Schema_named (pdlNameForElement @@ var "aliases" @@ true @@ var "name"))),
      _Type_wrap>>: lambda "wt" $
        -- Unwrap to inner type
        encodeType_ @@ var "cx" @@ var "g" @@ var "aliases" @@ var "wt",
      _Type_maybe>>: lambda "ot" $
        err (var "cx") (string "optionals unexpected at top level"),
      _Type_record>>: lambda "rt" $
        "rfields" <<~ (Eithers.mapList (encodeRecordField @@ var "cx" @@ var "g" @@ var "aliases") (var "rt")) $
        right (right (inject PDL._NamedSchemaType PDL._NamedSchemaType_record (record PDL._RecordSchema [
          PDL._RecordSchema_fields>>: var "rfields",
          PDL._RecordSchema_includes>>: list ([] :: [TTerm PDL.NamedSchema])]))),
      _Type_union>>: lambda "rt" $
        Logic.ifElse (Lists.foldl (lambda "b" $ lambda "t" $
            Logic.and (var "b") (Equality.equal (Strip.deannotateType @@ var "t") (MetaTypes.unit)))
          true (Lists.map (lambda "f" $ Core.fieldTypeType (var "f")) (var "rt")))
          -- Enum case
          ("fs" <<~ (Eithers.mapList (encodeEnumField @@ var "cx" @@ var "g") (var "rt")) $
           right (right (inject PDL._NamedSchemaType PDL._NamedSchemaType_enum (record PDL._EnumSchema [
             PDL._EnumSchema_fields>>: var "fs"]))))
          -- Union case
          ("members" <<~ (Eithers.mapList (encodeUnionField @@ var "cx" @@ var "g" @@ var "aliases") (var "rt")) $
           right (left (inject PDL._Schema PDL._Schema_union (wrap PDL._UnionSchema (var "members")))))]

encode :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> Type -> Either (InContext Error) PDL.Schema)
encode = def "encode" $
      "cx" ~> "g" ~> "aliases" ~> "t" ~>
        cases _Type (Strip.deannotateType @@ var "t")
          (Just $
            "res" <<~ (encodeType_ @@ var "cx" @@ var "g" @@ var "aliases" @@ var "t") $
            Eithers.either_
              (lambda "schema" $ right (var "schema"))
              (lambda "_" $ err (var "cx") (Strings.cat2 (string "type resolved to an unsupported nested named schema: ") (ShowCore.type_ @@ var "t")))
              (var "res")) [
          -- special case for the unit type
          _Type_record>>: lambda "rt" $
            Logic.ifElse (Lists.null (var "rt"))
              (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ MetaTypes.int32)
              ("res" <<~ (encodeType_ @@ var "cx" @@ var "g" @@ var "aliases" @@ var "t") $
               Eithers.either_
                 (lambda "schema" $ right (var "schema"))
                 (lambda "_" $ err (var "cx") (Strings.cat2 (string "type resolved to an unsupported nested named schema: ") (ShowCore.type_ @@ var "t")))
                 (var "res"))]

encodeRecordField :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> FieldType -> Either (InContext Error) PDL.RecordField)
encodeRecordField = def "encodeRecordField" $
  "cx" ~> "g" ~> "aliases" ~> "ft" ~>
    "name" <~ Core.fieldTypeName (var "ft") $
    "typ" <~ Core.fieldTypeType (var "ft") $
    "anns" <<~ (getAnns @@ var "cx" @@ var "g" @@ var "typ") $
    "optResult" <<~ (encodePossiblyOptionalType @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
    "schema" <~ Pairs.first (var "optResult") $
    "optional" <~ Pairs.second (var "optResult") $
    right (record PDL._RecordField [
      PDL._RecordField_name>>: wrap PDL._FieldName (Core.unName (var "name")),
      PDL._RecordField_value>>: var "schema",
      PDL._RecordField_optional>>: var "optional",
      PDL._RecordField_default>>: nothing,
      PDL._RecordField_annotations>>: var "anns"])

encodeUnionField :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> FieldType -> Either (InContext Error) PDL.UnionMember)
encodeUnionField = def "encodeUnionField" $
  "cx" ~> "g" ~> "aliases" ~> "ft" ~>
    "name" <~ Core.fieldTypeName (var "ft") $
    "typ" <~ Core.fieldTypeType (var "ft") $
    "anns" <<~ (getAnns @@ var "cx" @@ var "g" @@ var "typ") $
    "optResult" <<~ (encodePossiblyOptionalType @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
    "s" <~ Pairs.first (var "optResult") $
    "optional" <~ Pairs.second (var "optResult") $
    "schema" <~ Logic.ifElse (var "optional")
      (inject PDL._Schema PDL._Schema_union (wrap PDL._UnionSchema (Lists.map (lambda "ms" $ simpleUnionMember @@ var "ms") (list [inject PDL._Schema PDL._Schema_null (unit), var "s"]))))
      (var "s") $
    right (record PDL._UnionMember [
      PDL._UnionMember_alias>>: just (wrap PDL._FieldName (Core.unName (var "name"))),
      PDL._UnionMember_value>>: var "schema",
      PDL._UnionMember_annotations>>: var "anns"])

encodeEnumField :: TTermDefinition (Context -> Graph -> FieldType -> Either (InContext Error) PDL.EnumField)
encodeEnumField = def "encodeEnumField" $
  "cx" ~> "g" ~> "ft" ~>
    "name" <~ Core.fieldTypeName (var "ft") $
    "typ" <~ Core.fieldTypeType (var "ft") $
    "anns" <<~ (getAnns @@ var "cx" @@ var "g" @@ var "typ") $
    right (record PDL._EnumField [
      PDL._EnumField_name>>: wrap PDL._EnumFieldName (Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionUpperSnake @@ Core.unName (var "name")),
      PDL._EnumField_annotations>>: var "anns"])

encodePossiblyOptionalType :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> Type -> Either (InContext Error) (PDL.Schema, Bool))
encodePossiblyOptionalType = def "encodePossiblyOptionalType" $
  "cx" ~> "g" ~> "aliases" ~> "typ" ~>
    cases _Type (Strip.deannotateType @@ var "typ") Nothing [
      _Type_maybe>>: lambda "ot" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "ot") $
        right (pair (var "t") true),
      _Type_record>>: lambda "rt" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_union>>: lambda "ut" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_literal>>: lambda "lt" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_list>>: lambda "lt" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_map>>: lambda "mt" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_set>>: lambda "st" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_variable>>: lambda "name" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_wrap>>: lambda "wt" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_either>>: lambda "et" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_pair>>: lambda "pt" $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_void>>: constant $
        "t" <<~ (encode @@ var "cx" @@ var "g" @@ var "aliases" @@ var "typ") $
        right (pair (var "t") false),
      _Type_annotated>>: lambda "at" $
        encodePossiblyOptionalType @@ var "cx" @@ var "g" @@ var "aliases" @@ Core.annotatedTypeBody (var "at")]

getAnns :: TTermDefinition (Context -> Graph -> Type -> Either (InContext Error) PDL.Annotations)
getAnns = def "getAnns" $
  "cx" ~> "g" ~> "typ" ~>
    "r" <<~ (Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "typ") $
    right (doc_ @@ var "r")


importAliasesForModule :: TTermDefinition (Context -> Graph -> Module -> Either (InContext Error) (M.Map Namespace String))
importAliasesForModule = def "importAliasesForModule" $
  doc "Compute import aliases for a module's dependencies" $
  "cx" ~> "g" ~> "mod" ~>
    "nss" <<~ (Analysis.moduleDependencyNamespaces @@ var "cx" @@ var "g" @@ false @@ true @@ true @@ false @@ var "mod") $
    right (Maps.fromList (Lists.map
      (lambda "ns_" $ pair (var "ns_") (slashesToDots @@ (unwrap _Namespace @@ var "ns_")))
      (Sets.toList (var "nss"))))


noAnnotations_ :: TTermDefinition (PDL.Annotations)
noAnnotations_ = def "noAnnotations" $
  doc "Empty PDL annotations" $
  record PDL._Annotations [
    PDL._Annotations_doc>>: nothing,
    PDL._Annotations_deprecated>>: false]


pdlNameForElement :: TTermDefinition (M.Map Namespace String -> Bool -> Name -> PDL.QualifiedName)
pdlNameForElement = def "pdlNameForElement" $
  doc "Convert a Hydra element name to a PDL qualified name" $
  "aliases" ~> "withNs" ~> "name" ~>
    "qn" <~ (Names.qualifyName @@ var "name") $
    "ns_" <~ project _QualifiedName _QualifiedName_namespace @@ var "qn" $
    "local" <~ project _QualifiedName _QualifiedName_local @@ var "qn" $
    "alias" <~ Maybes.bind (var "ns_") (lambda "n" $ Maps.lookup (var "n") (var "aliases")) $
    record PDL._QualifiedName [
      PDL._QualifiedName_name>>: wrap PDL._Name (var "local"),
      PDL._QualifiedName_namespace>>: Logic.ifElse (var "withNs")
        (Maybes.map (lambda "a" $ wrap PDL._Namespace (var "a")) (var "alias"))
        nothing]


pdlNameForModule :: TTermDefinition (Module -> PDL.Namespace)
pdlNameForModule = def "pdlNameForModule" $
  doc "Convert a module's namespace to a PDL namespace" $
  "mod" ~> wrap PDL._Namespace (slashesToDots @@ (unwrap _Namespace @@ Packaging.moduleNamespace (var "mod")))


simpleUnionMember :: TTermDefinition (PDL.Schema -> PDL.UnionMember)
simpleUnionMember = def "simpleUnionMember" $
  doc "Create a simple union member without an alias" $
  "schema" ~> record PDL._UnionMember [
    PDL._UnionMember_alias>>: nothing,
    PDL._UnionMember_value>>: var "schema",
    PDL._UnionMember_annotations>>: noAnnotations_]


slashesToDots :: TTermDefinition (String -> String)
slashesToDots = def "slashesToDots" $
  doc "Replace all forward slashes with dots in a string" $
  "s" ~> Strings.intercalate (string ".") (Strings.splitOn (string "/") (var "s"))
