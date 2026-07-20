{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Avro.SchemaJson where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (Result)
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms                      as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants                   as Variants
import qualified Hydra.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular                         as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
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
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants  as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Scientific                           as Sci
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Avro.Schema as Avro
import qualified Hydra.Json.Model as JM
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Sources.Avro.Schema as AvroSchema
-- Result type alias (was previously imported from Staging module)
type Result a = Either Error a


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.avro.schemaJson"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([jsonWriterNs, jsonParserNs] L.++ (AvroSchema.ns:jsonModelNs:ModuleName "hydra.parsing":KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "JSON serialization and deserialization for Avro schemas")}
  where
    definitions = [
      toDefinition avroAliases,
      toDefinition avroArray,
      toDefinition avroAscending,
      toDefinition avroBoolean,
      toDefinition avroBytes,
      toDefinition avroDefault,
      toDefinition avroDescending,
      toDefinition avroDoc,
      toDefinition avroDouble,
      toDefinition avroEnum,
      toDefinition avroFields,
      toDefinition avroFixed,
      toDefinition avroFloat,
      toDefinition avroIgnore,
      toDefinition avroInt,
      toDefinition avroItems,
      toDefinition avroLong,
      toDefinition avroMap,
      toDefinition avroName,
      toDefinition avroNamespace,
      toDefinition avroNull,
      toDefinition avroOrder,
      toDefinition avroRecord,
      toDefinition avroSchemaJsonCoder,
      toDefinition avroSchemaStringCoder,
      toDefinition avroSize,
      toDefinition avroString,
      toDefinition avroSymbols,
      toDefinition avroType,
      toDefinition avroValues,
      toDefinition decodeAliases,
      toDefinition decodeArraySchema,
      toDefinition decodeEnum,
      toDefinition decodeField,
      toDefinition decodeFixed,
      toDefinition decodeMapSchema,
      toDefinition decodeNamedSchema,
      toDefinition decodeObjectSchema,
      toDefinition decodeOrder,
      toDefinition decodePrimitiveName,
      toDefinition decodeRecord,
      toDefinition decodeSchema,
      toDefinition encodeAnnotations,
      toDefinition encodeArray,
      toDefinition encodeEnumE,
      toDefinition encodeFieldE,
      toDefinition encodeFixedE,
      toDefinition encodeMap,
      toDefinition encodeNamed,
      toDefinition encodeNamedType,
      toDefinition encodeOrderE,
      toDefinition encodePrimitive,
      toDefinition encodeRecordE,
      toDefinition encodeSchema,
      toDefinition encodeUnion,
      toDefinition err,
      toDefinition expectArrayE,
      toDefinition expectNumberE,
      toDefinition expectObjectE,
      toDefinition expectStringE,
      toDefinition getAnnotations,
      toDefinition optArrayE,
      toDefinition optE,
      toDefinition optStringE,
      toDefinition requireArrayE,
      toDefinition requireE,
      toDefinition requireNumberE,
      toDefinition requireStringE,
      toDefinition showJsonValue,
      toDefinition stringToJsonValue,
      toDefinition unexpectedE]


-- | Error helpers

avroSchemaJsonCoder :: TypedTermDefinition (InferenceContext -> Coder Avro.Schema JM.Value Error)
avroSchemaJsonCoder = define "avroSchemaJsonCoder" $
  doc "Create a coder between Avro schemas and JSON values" $
  lambda "cx" $
    record _Coder [
      _Coder_encode>>: lambda "schema" $ Phantoms.right (encodeSchema @@ var "schema"),
      _Coder_decode>>: lambda "json" $ decodeSchema @@ var "cx" @@ var "json"]

avroSchemaPhantomNs :: ModuleName
avroSchemaPhantomNs = ModuleName "hydra.avro.schema"

avroSchemaStringCoder :: TypedTermDefinition (InferenceContext -> Coder Avro.Schema String Error)
avroSchemaStringCoder = define "avroSchemaStringCoder" $
  doc "Create a coder between Avro schemas and JSON strings" $
  lambda "cx" $
    record _Coder [
      _Coder_encode>>: lambda "schema" $
        Phantoms.right (showJsonValue @@ (encodeSchema @@ var "schema")),
      _Coder_decode>>: lambda "s" $
        Eithers.bind
          (Eithers.either
            (lambda "e" $ err @@ var "cx" @@ var "e")
            (lambda "v" $ Phantoms.right (var "v"))
            (stringToJsonValue @@ var "s"))
          (lambda "json" $ decodeSchema @@ var "cx" @@ var "json")]


-- | Decode functions

avroAliases :: TypedTermDefinition String
avroAliases = define "avroAliases" $
  doc "The JSON attribute name for the list of alternate names of a named type or field" $
  string "aliases"

avroArray :: TypedTermDefinition String
avroArray = define "avroArray" $
  doc "The JSON type-name value identifying an Avro array schema" $
  string "array"

avroAscending :: TypedTermDefinition String
avroAscending = define "avroAscending" $
  doc "The JSON value identifying ascending field order" $
  string "ascending"

avroBoolean :: TypedTermDefinition String
avroBoolean = define "avroBoolean" $
  doc "The JSON type-name value identifying the Avro boolean primitive" $
  string "boolean"

avroBytes :: TypedTermDefinition String
avroBytes = define "avroBytes" $
  doc "The JSON type-name value identifying the Avro bytes primitive" $
  string "bytes"

avroDefault :: TypedTermDefinition String
avroDefault = define "avroDefault" $
  doc "The JSON attribute name for a field's or enum's default value" $
  string "default"

avroDescending :: TypedTermDefinition String
avroDescending = define "avroDescending" $
  doc "The JSON value identifying descending field order" $
  string "descending"

avroDoc :: TypedTermDefinition String
avroDoc = define "avroDoc" $
  doc "The JSON attribute name for a named type's or field's documentation string" $
  string "doc"

avroDouble :: TypedTermDefinition String
avroDouble = define "avroDouble" $
  doc "The JSON type-name value identifying the Avro double primitive" $
  string "double"

avroEnum :: TypedTermDefinition String
avroEnum = define "avroEnum" $
  doc "The JSON type-name value identifying an Avro enum schema" $
  string "enum"

avroFields :: TypedTermDefinition String
avroFields = define "avroFields" $
  doc "The JSON attribute name for a record's list of fields" $
  string "fields"

avroFixed :: TypedTermDefinition String
avroFixed = define "avroFixed" $
  doc "The JSON type-name value identifying an Avro fixed schema" $
  string "fixed"

avroFloat :: TypedTermDefinition String
avroFloat = define "avroFloat" $
  doc "The JSON type-name value identifying the Avro float primitive" $
  string "float"

avroIgnore :: TypedTermDefinition String
avroIgnore = define "avroIgnore" $
  doc "The JSON value identifying ignored field order" $
  string "ignore"

avroInt :: TypedTermDefinition String
avroInt = define "avroInt" $
  doc "The JSON type-name value identifying the Avro int primitive" $
  string "int"

avroItems :: TypedTermDefinition String
avroItems = define "avroItems" $
  doc "The JSON attribute name for an array schema's item type" $
  string "items"

avroLong :: TypedTermDefinition String
avroLong = define "avroLong" $
  doc "The JSON type-name value identifying the Avro long primitive" $
  string "long"

avroMap :: TypedTermDefinition String
avroMap = define "avroMap" $
  doc "The JSON type-name value identifying an Avro map schema" $
  string "map"

avroName :: TypedTermDefinition String
avroName = define "avroName" $
  doc "The JSON attribute name for a named type's or field's name" $
  string "name"

avroNamespace :: TypedTermDefinition String
avroNamespace = define "avroNamespace" $
  doc "The JSON attribute name for a named type's namespace" $
  string "namespace"

avroNull :: TypedTermDefinition String
avroNull = define "avroNull" $
  doc "The JSON type-name value identifying the Avro null primitive" $
  string "null"

avroOrder :: TypedTermDefinition String
avroOrder = define "avroOrder" $
  doc "The JSON attribute name for a field's sort order" $
  string "order"

avroRecord :: TypedTermDefinition String
avroRecord = define "avroRecord" $
  doc "The JSON type-name value identifying an Avro record schema" $
  string "record"

avroSize :: TypedTermDefinition String
avroSize = define "avroSize" $
  doc "The JSON attribute name for a fixed schema's byte size" $
  string "size"

avroString :: TypedTermDefinition String
avroString = define "avroString" $
  doc "The JSON type-name value identifying the Avro string primitive" $
  string "string"

avroSymbols :: TypedTermDefinition String
avroSymbols = define "avroSymbols" $
  doc "The JSON attribute name for an enum schema's list of symbols" $
  string "symbols"

avroType :: TypedTermDefinition String
avroType = define "avroType" $
  doc "The JSON attribute name for a schema's type name" $
  string "type"

avroValues :: TypedTermDefinition String
avroValues = define "avroValues" $
  doc "The JSON attribute name for a map schema's value type" $
  string "values"


-- | Encode functions

decodeAliases :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> Result (Maybe [String]))
decodeAliases = define "decodeAliases" $
  doc "Decode aliases from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (optArrayE @@ var "cx" @@ avroAliases @@ var "m")
      (lambda "mArr" $
        Optionals.cases (var "mArr") (Phantoms.right nothing) (lambda "arr" $
            Eithers.map
              (lambda "strs" $ Optionals.pure (var "strs"))
              (Eithers.mapList (expectStringE @@ var "cx") (var "arr"))))

decodeArraySchema :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> Result Avro.Schema)
decodeArraySchema = define "decodeArraySchema" $
  doc "Decode an Avro array schema from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ avroItems @@ var "m")
      (lambda "items" $
        Eithers.map
          (lambda "s" $ inject Avro._Schema Avro._Schema_array (record Avro._Array [
            Avro._Array_items>>: var "s"]))
          (decodeSchema @@ var "cx" @@ var "items"))

decodeEnum :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> Result Avro.NamedType)
decodeEnum = define "decodeEnum" $
  doc "Decode an Avro enum type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireArrayE @@ var "cx" @@ avroSymbols @@ var "m")
      (lambda "syms" $
        Eithers.bind (Eithers.mapList (expectStringE @@ var "cx") (var "syms"))
          (lambda "symbols" $
            Eithers.bind (optStringE @@ var "cx" @@ avroDefault @@ var "m")
              (lambda "defVal" $
                Phantoms.right (inject Avro._NamedType Avro._NamedType_enum
                  (record Avro._Enum [
                    Avro._Enum_symbols>>: var "symbols",
                    Avro._Enum_default>>: var "defVal"])))))

decodeField :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> Result Avro.Field)
decodeField = define "decodeField" $
  doc "Decode an Avro field from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireStringE @@ var "cx" @@ avroName @@ var "m")
      (lambda "name" $
        Eithers.bind (optStringE @@ var "cx" @@ avroDoc @@ var "m")
          (lambda "fdoc" $
            Eithers.bind (requireE @@ var "cx" @@ avroType @@ var "m")
              (lambda "typeJson" $
                Eithers.bind (decodeSchema @@ var "cx" @@ var "typeJson")
                  (lambda "fieldType" $
                    Eithers.bind (Eithers.bind (optStringE @@ var "cx" @@ avroOrder @@ var "m")
                      (lambda "mOrd" $
                        Eithers.mapOptional (decodeOrder @@ var "cx") (var "mOrd")))
                      (lambda "order" $
                        Eithers.bind (decodeAliases @@ var "cx" @@ var "m")
                          (lambda "aliases" $
                            Phantoms.right (record Avro._Field [
                              Avro._Field_name>>: var "name",
                              Avro._Field_doc>>: var "fdoc",
                              Avro._Field_type>>: var "fieldType",
                              Avro._Field_default>>: optE @@ avroDefault @@ var "m",
                              Avro._Field_order>>: var "order",
                              Avro._Field_aliases>>: var "aliases",
                              Avro._Field_annotations>>: getAnnotations @@ var "m"])))))))

decodeFixed :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> Result Avro.NamedType)
decodeFixed = define "decodeFixed" $
  doc "Decode an Avro fixed type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireNumberE @@ var "cx" @@ avroSize @@ var "m")
      (lambda "n" $
        lets ["size">: Literals.bigintToInt32 (Literals.decimalToBigint (var "n"))] $
        Phantoms.right $ inject Avro._NamedType Avro._NamedType_fixed $
          record Avro._Fixed [
            Avro._Fixed_size>>: var "size"])

decodeMapSchema :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> Result Avro.Schema)
decodeMapSchema = define "decodeMapSchema" $
  doc "Decode an Avro map schema from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ avroValues @@ var "m")
      (lambda "values" $
        Eithers.map
          (lambda "s" $ inject Avro._Schema Avro._Schema_map (record Avro._Map [
            Avro._Map_values>>: var "s"]))
          (decodeSchema @@ var "cx" @@ var "values"))

decodeNamedSchema :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> Result Avro.NamedType -> Result Avro.Schema)
decodeNamedSchema = define "decodeNamedSchema" $
  doc "Decode a named Avro schema from a JSON object map and a decoded named type result" $
  lambda "cx" $ lambda "m" $ lambda "namedTypeResult" $
    Eithers.bind (requireStringE @@ var "cx" @@ avroName @@ var "m")
      (lambda "name" $
        Eithers.bind (optStringE @@ var "cx" @@ avroNamespace @@ var "m")
          (lambda "ns" $
            Eithers.bind (optStringE @@ var "cx" @@ avroDoc @@ var "m")
              (lambda "sdoc" $
                Eithers.bind (decodeAliases @@ var "cx" @@ var "m")
                  (lambda "aliases" $
                    Eithers.bind (var "namedTypeResult")
                      (lambda "namedType" $
                        Phantoms.right (inject Avro._Schema Avro._Schema_named
                          (record Avro._Named [
                            Avro._Named_name>>: var "name",
                            Avro._Named_namespace>>: var "ns",
                            Avro._Named_aliases>>: var "aliases",
                            Avro._Named_doc>>: var "sdoc",
                            Avro._Named_type>>: var "namedType",
                            Avro._Named_annotations>>: getAnnotations @@ var "m"])))))))

decodeObjectSchema :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> String -> Result Avro.Schema)
decodeObjectSchema = define "decodeObjectSchema" $
  doc "Decode an Avro schema from a JSON object given the type name" $
  lambda "cx" $ lambda "m" $ lambda "typeName" $
    Logic.ifElse (Equality.equal (var "typeName") (string "array"))
      (decodeArraySchema @@ var "cx" @@ var "m")
      (Logic.ifElse (Equality.equal (var "typeName") (string "map"))
        (decodeMapSchema @@ var "cx" @@ var "m")
        (Logic.ifElse (Equality.equal (var "typeName") (string "record"))
          (decodeNamedSchema @@ var "cx" @@ var "m" @@ (decodeRecord @@ var "cx" @@ var "m"))
          (Logic.ifElse (Equality.equal (var "typeName") (string "enum"))
            (decodeNamedSchema @@ var "cx" @@ var "m" @@ (decodeEnum @@ var "cx" @@ var "m"))
            (Logic.ifElse (Equality.equal (var "typeName") (string "fixed"))
              (decodeNamedSchema @@ var "cx" @@ var "m" @@ (decodeFixed @@ var "cx" @@ var "m"))
              -- Primitive type as object (unusual but valid, e.g. {"type": "string"})
              (Optionals.cases (decodePrimitiveName @@ var "typeName") (err @@ var "cx" @@ (Strings.cat $ list [string "unknown type: ", var "typeName"])) (lambda "p" $ Phantoms.right (inject Avro._Schema Avro._Schema_primitive (var "p"))))))))

decodeOrder :: TypedTermDefinition (InferenceContext -> String -> Result Avro.Order)
decodeOrder = define "decodeOrder" $
  doc "Decode an Avro field ordering from a string" $
  lambda "cx" $ lambda "o" $
    Logic.ifElse (Equality.equal (var "o") (string "ascending"))
      (Phantoms.right (inject Avro._Order Avro._Order_ascending unit))
      (Logic.ifElse (Equality.equal (var "o") (string "descending"))
        (Phantoms.right (inject Avro._Order Avro._Order_descending unit))
        (Logic.ifElse (Equality.equal (var "o") (string "ignore"))
          (Phantoms.right (inject Avro._Order Avro._Order_ignore unit))
          (err @@ var "cx" @@ (Strings.cat $ list [string "unknown order: ", var "o"]))))

decodePrimitiveName :: TypedTermDefinition (String -> Maybe Avro.Primitive)
decodePrimitiveName = define "decodePrimitiveName" $
  doc "Decode a primitive type name string to a Primitive, or Nothing if not a primitive" $
  lambda "s" $
    Logic.ifElse (Equality.equal (var "s") (string "null"))
      (just (inject Avro._Primitive Avro._Primitive_null unit))
      (Logic.ifElse (Equality.equal (var "s") (string "boolean"))
        (just (inject Avro._Primitive Avro._Primitive_boolean unit))
        (Logic.ifElse (Equality.equal (var "s") (string "int"))
          (just (inject Avro._Primitive Avro._Primitive_int unit))
          (Logic.ifElse (Equality.equal (var "s") (string "long"))
            (just (inject Avro._Primitive Avro._Primitive_long unit))
            (Logic.ifElse (Equality.equal (var "s") (string "float"))
              (just (inject Avro._Primitive Avro._Primitive_float unit))
              (Logic.ifElse (Equality.equal (var "s") (string "double"))
                (just (inject Avro._Primitive Avro._Primitive_double unit))
                (Logic.ifElse (Equality.equal (var "s") (string "bytes"))
                  (just (inject Avro._Primitive Avro._Primitive_bytes unit))
                  (Logic.ifElse (Equality.equal (var "s") (string "string"))
                    (just (inject Avro._Primitive Avro._Primitive_string unit))
                    nothing)))))))

decodeRecord :: TypedTermDefinition (InferenceContext -> M.Map String JM.Value -> Result Avro.NamedType)
decodeRecord = define "decodeRecord" $
  doc "Decode an Avro record type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    Eithers.bind (requireArrayE @@ var "cx" @@ avroFields @@ var "m")
      (lambda "fieldJsons" $
        Eithers.bind (Eithers.mapList
          (lambda "fj" $
            Eithers.bind (expectObjectE @@ var "cx" @@ var "fj")
              (lambda "fm" $ decodeField @@ var "cx" @@ var "fm"))
          (var "fieldJsons"))
          (lambda "fields" $
            Phantoms.right (inject Avro._NamedType Avro._NamedType_record
              (record Avro._Record [
                Avro._Record_fields>>: var "fields"]))))

decodeSchema :: TypedTermDefinition (InferenceContext -> JM.Value -> Result Avro.Schema)
decodeSchema = define "decodeSchema" $
  doc "Decode an Avro schema from a JSON value" $
  lambda "cx" $ lambda "v" $
    cases JM._Value (var "v") (Just (err @@ var "cx" @@ (Strings.cat $ list [string "unexpected JSON value for schema: ", showJsonValue @@ var "v"]))) [
      -- String: primitive type name or reference
      JM._Value_string>>: lambda "s" $
        Optionals.cases (decodePrimitiveName @@ var "s") (Phantoms.right (inject Avro._Schema Avro._Schema_reference (var "s"))) (lambda "p" $ Phantoms.right (inject Avro._Schema Avro._Schema_primitive (var "p"))),
      -- Array: union type
      JM._Value_array>>: lambda "schemas" $
        Eithers.map
          (lambda "decoded" $ inject Avro._Schema Avro._Schema_union (wrap Avro._Union (var "decoded")))
          (Eithers.mapList (decodeSchema @@ var "cx") (var "schemas")),
      -- Object: named type or container. The object payload is an ordered pair-list;
      -- decoding looks fields up by name, so convert to a map up front.
      JM._Value_object>>: lambda "mList" $ lets [
        "m">: (Maps.fromList (var "mList") :: TypedTerm (M.Map String JM.Value))] $
        Eithers.bind (requireStringE @@ var "cx" @@ avroType @@ var "m")
          (lambda "typeName" $ decodeObjectSchema @@ var "cx" @@ var "m" @@ var "typeName")]

encodeAnnotations :: TypedTermDefinition (M.Map String JM.Value -> [(String, JM.Value)])
encodeAnnotations = define "encodeAnnotations" $
  doc "Encode annotations as key-value pairs with @ prefix on keys" $
  lambda "m" $
    Lists.map
      (lambda "entry" $ pair
        (Strings.cat2 (string "@") (Pairs.first (var "entry")))
        (Pairs.second (var "entry")))
      (Maps.toList (var "m" :: TypedTerm (M.Map String JM.Value)))


-- | Coder functions

encodeArray :: TypedTermDefinition (Avro.Array -> JM.Value)
encodeArray = define "encodeArray" $
  doc "Encode an Avro array schema to a JSON object" $
  lambda "arr" $
    inject JM._Value JM._Value_object
      (list [
        pair (string "type") (inject JM._Value JM._Value_string (string "array")),
        pair (string "items") (encodeSchema @@ (project Avro._Array Avro._Array_items @@ var "arr"))])

encodeEnumE :: TypedTermDefinition (Avro.Enum -> [(String, JM.Value)])
encodeEnumE = define "encodeEnum" $
  doc "Encode an Avro enum type as key-value pairs" $
  lambda "e" $
    Lists.concat (list [
      list [pair (string "type") (inject JM._Value JM._Value_string (string "enum"))],
      list [pair (string "symbols") (inject JM._Value JM._Value_array (Lists.map (lambda "s" $ inject JM._Value JM._Value_string (var "s")) (project Avro._Enum Avro._Enum_symbols @@ var "e")))],
      Optionals.cases (project Avro._Enum Avro._Enum_default @@ var "e") (list ([] :: [TypedTerm (String, JM.Value)])) (lambda "d" $ list [pair (string "default") (inject JM._Value JM._Value_string (var "d"))])])

encodeFieldE :: TypedTermDefinition (Avro.Field -> JM.Value)
encodeFieldE = define "encodeField" $
  doc "Encode an Avro field to a JSON object" $
  lambda "f" $
    inject JM._Value JM._Value_object
      (Lists.concat (list [
        list [pair (string "name") (inject JM._Value JM._Value_string (project Avro._Field Avro._Field_name @@ var "f"))],
        list [pair (string "type") (encodeSchema @@ (project Avro._Field Avro._Field_type @@ var "f"))],
        Optionals.cases (project Avro._Field Avro._Field_doc @@ var "f") (list ([] :: [TypedTerm (String, JM.Value)])) (lambda "d" $ list [pair (string "doc") (inject JM._Value JM._Value_string (var "d"))]),
        Optionals.cases (project Avro._Field Avro._Field_default @@ var "f") (list ([] :: [TypedTerm (String, JM.Value)])) (lambda "d" $ list [pair (string "default") (var "d")]),
        Optionals.cases (project Avro._Field Avro._Field_order @@ var "f") (list ([] :: [TypedTerm (String, JM.Value)])) (lambda "o" $ list [encodeOrderE @@ var "o"]),
        Optionals.cases (project Avro._Field Avro._Field_aliases @@ var "f") (list ([] :: [TypedTerm (String, JM.Value)])) (lambda "als" $ list [pair (string "aliases") (inject JM._Value JM._Value_array (Lists.map (lambda "a" $ inject JM._Value JM._Value_string (var "a")) (var "als")))]),
        encodeAnnotations @@ (project Avro._Field Avro._Field_annotations @@ var "f")]))

encodeFixedE :: TypedTermDefinition (Avro.Fixed -> [(String, JM.Value)])
encodeFixedE = define "encodeFixed" $
  doc "Encode an Avro fixed type as key-value pairs" $
  lambda "f" $
    list [
      pair (string "type") (inject JM._Value JM._Value_string (string "fixed")),
      pair (string "size") (inject JM._Value JM._Value_number (Literals.bigintToDecimal (Literals.int32ToBigint (project Avro._Fixed Avro._Fixed_size @@ var "f"))))]

encodeMap :: TypedTermDefinition (Avro.Map -> JM.Value)
encodeMap = define "encodeMap" $
  doc "Encode an Avro map schema to a JSON object" $
  lambda "mp" $
    inject JM._Value JM._Value_object
      (list [
        pair (string "type") (inject JM._Value JM._Value_string (string "map")),
        pair (string "values") (encodeSchema @@ (project Avro._Map Avro._Map_values @@ var "mp"))])

encodeNamed :: TypedTermDefinition (Avro.Named -> JM.Value)
encodeNamed = define "encodeNamed" $
  doc "Encode an Avro named type to a JSON object" $
  lambda "n" $
    inject JM._Value JM._Value_object
      (Lists.concat (list [
        list [pair (string "name") (inject JM._Value JM._Value_string (project Avro._Named Avro._Named_name @@ var "n"))],
        Optionals.cases (project Avro._Named Avro._Named_namespace @@ var "n") (list ([] :: [TypedTerm (String, JM.Value)])) (lambda "ns" $ list [pair (string "namespace") (inject JM._Value JM._Value_string (var "ns"))]),
        Optionals.cases (project Avro._Named Avro._Named_doc @@ var "n") (list ([] :: [TypedTerm (String, JM.Value)])) (lambda "d" $ list [pair (string "doc") (inject JM._Value JM._Value_string (var "d"))]),
        Optionals.cases (project Avro._Named Avro._Named_aliases @@ var "n") (list ([] :: [TypedTerm (String, JM.Value)])) (lambda "als" $ list [pair (string "aliases") (inject JM._Value JM._Value_array (Lists.map (lambda "a" $ inject JM._Value JM._Value_string (var "a")) (var "als")))]),
        encodeNamedType @@ (project Avro._Named Avro._Named_type @@ var "n"),
        encodeAnnotations @@ (project Avro._Named Avro._Named_annotations @@ var "n")]))

encodeNamedType :: TypedTermDefinition (Avro.NamedType -> [(String, JM.Value)])
encodeNamedType = define "encodeNamedType" $
  doc "Encode the specific variant of a named Avro type" $
  lambda "nt" $
    cases Avro._NamedType (var "nt") Nothing [
      Avro._NamedType_enum>>: lambda "e" $ encodeEnumE @@ var "e",
      Avro._NamedType_fixed>>: lambda "f" $ encodeFixedE @@ var "f",
      Avro._NamedType_record>>: lambda "r" $ encodeRecordE @@ var "r"]

encodeOrderE :: TypedTermDefinition (Avro.Order -> (String, JM.Value))
encodeOrderE = define "encodeOrder" $
  doc "Encode an Avro field ordering as a key-value pair" $
  lambda "o" $
    pair (string "order") (inject JM._Value JM._Value_string
      (cases Avro._Order (var "o") Nothing [
        Avro._Order_ascending>>: constant (string "ascending"),
        Avro._Order_descending>>: constant (string "descending"),
        Avro._Order_ignore>>: constant (string "ignore")]))

encodePrimitive :: TypedTermDefinition (Avro.Primitive -> JM.Value)
encodePrimitive = define "encodePrimitive" $
  doc "Encode an Avro primitive type as a JSON string" $
  lambda "p" $
    inject JM._Value JM._Value_string
      (cases Avro._Primitive (var "p") Nothing [
        Avro._Primitive_null>>: constant (string "null"),
        Avro._Primitive_boolean>>: constant (string "boolean"),
        Avro._Primitive_int>>: constant (string "int"),
        Avro._Primitive_long>>: constant (string "long"),
        Avro._Primitive_float>>: constant (string "float"),
        Avro._Primitive_double>>: constant (string "double"),
        Avro._Primitive_bytes>>: constant (string "bytes"),
        Avro._Primitive_string>>: constant (string "string")])

encodeRecordE :: TypedTermDefinition (Avro.Record -> [(String, JM.Value)])
encodeRecordE = define "encodeRecord" $
  doc "Encode an Avro record type as key-value pairs" $
  lambda "r" $
    list [
      pair (string "type") (inject JM._Value JM._Value_string (string "record")),
      pair (string "fields") (inject JM._Value JM._Value_array (Lists.map (asTerm encodeFieldE) (project Avro._Record Avro._Record_fields @@ var "r")))]

encodeSchema :: TypedTermDefinition (Avro.Schema -> JM.Value)
encodeSchema = define "encodeSchema" $
  doc "Encode an Avro schema to a JSON value" $
  lambda "schema" $
    cases Avro._Schema (var "schema") Nothing [
      Avro._Schema_primitive>>: lambda "p" $ encodePrimitive @@ var "p",
      Avro._Schema_array>>: lambda "arr" $ encodeArray @@ var "arr",
      Avro._Schema_map>>: lambda "mp" $ encodeMap @@ var "mp",
      Avro._Schema_named>>: lambda "n" $ encodeNamed @@ var "n",
      Avro._Schema_reference>>: lambda "ref" $ inject JM._Value JM._Value_string (var "ref"),
      Avro._Schema_union>>: lambda "u" $ encodeUnion @@ var "u"]

encodeUnion :: TypedTermDefinition (Avro.Union -> JM.Value)
encodeUnion = define "encodeUnion" $
  doc "Encode an Avro union as a JSON array of schemas" $
  lambda "u" $
    inject JM._Value JM._Value_array
      (Lists.map (asTerm encodeSchema) (unwrap Avro._Union @@ var "u"))

err :: TypedTermDefinition (InferenceContext -> String -> Result a)
err = define "err" $
  doc "Construct an error result with a message in context" $
  lambda "cx" $ lambda "msg" $
    left (Error.errorOther $ Error.otherError (var "msg"))

expectArrayE :: TypedTermDefinition (InferenceContext -> JM.Value -> Result [JM.Value])
expectArrayE = define "expectArrayE" $
  doc "Extract a JSON array or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_array>>: lambda "v" $ Phantoms.right (var "v")]

expectNumberE :: TypedTermDefinition (InferenceContext -> JM.Value -> Result Sci.Scientific)
expectNumberE = define "expectNumberE" $
  doc "Extract a JSON number or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_number>>: lambda "v" $ Phantoms.right (var "v")]

expectObjectE :: TypedTermDefinition (InferenceContext -> JM.Value -> Result (M.Map String JM.Value))
expectObjectE = define "expectObjectE" $
  doc "Extract a JSON object as a name-keyed map or return an error (field order is dropped)" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_object>>: lambda "v" $ Phantoms.right (Maps.fromList (var "v") :: TypedTerm (M.Map String JM.Value))]

expectStringE :: TypedTermDefinition (InferenceContext -> JM.Value -> Result String)
expectStringE = define "expectStringE" $
  doc "Extract a JSON string or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_string>>: lambda "v" $ Phantoms.right (var "v")]

getAnnotations :: TypedTermDefinition (M.Map String JM.Value -> M.Map String JM.Value)
getAnnotations = define "getAnnotations" $
  doc "Extract annotation entries (keys starting with @) from a JSON object map" $
  lambda "m" $
    (Maps.fromList (Optionals.cat (Lists.map
      (lambda "entry" $ lets [
        "k">: Pairs.first (var "entry"),
        "v">: Pairs.second (var "entry")] $
        Logic.ifElse
          (Equality.equal (Optionals.fromOptional (int32 0) (Strings.maybeCharAt (int32 0) (var "k"))) (int32 64))  -- 64 = '@'
          (Optionals.pure (pair (Strings.fromList (Lists.drop (int32 1) (Strings.toList (var "k")))) (var "v")))
          nothing)
      (Maps.toList (var "m" :: TypedTerm (M.Map String JM.Value))))) :: TypedTerm (M.Map String JM.Value))

jsonModelNs :: ModuleName
jsonModelNs = ModuleName "hydra.json.model"

jsonParserNs :: ModuleName
jsonParserNs = ModuleName "hydra.json.parser"

jsonWriterNs :: ModuleName
jsonWriterNs = ModuleName "hydra.json.writer"

optArrayE :: TypedTermDefinition (InferenceContext -> String -> M.Map String JM.Value -> Result (Maybe [JM.Value]))
optArrayE = define "optArrayE" $
  doc "Look up an optional array attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Optionals.cases (Maps.lookup (var "fname") (var "m" :: TypedTerm (M.Map String JM.Value))) (Phantoms.right nothing) (lambda "v" $ Eithers.map (lambda "a" $ Optionals.pure (var "a")) (expectArrayE @@ var "cx" @@ var "v"))

optE :: TypedTermDefinition (String -> M.Map String JM.Value -> Maybe JM.Value)
optE = define "optE" $
  doc "Look up an optional attribute in a JSON object map" $
  lambda "k" $ lambda "m" $
    Maps.lookup (var "k") (var "m" :: TypedTerm (M.Map String JM.Value))

optStringE :: TypedTermDefinition (InferenceContext -> String -> M.Map String JM.Value -> Result (Maybe String))
optStringE = define "optStringE" $
  doc "Look up an optional string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Optionals.cases (Maps.lookup (var "fname") (var "m" :: TypedTerm (M.Map String JM.Value))) (Phantoms.right nothing) (lambda "v" $ Eithers.map (lambda "s" $ Optionals.pure (var "s")) (expectStringE @@ var "cx" @@ var "v"))

requireArrayE :: TypedTermDefinition (InferenceContext -> String -> M.Map String JM.Value -> Result [JM.Value])
requireArrayE = define "requireArrayE" $
  doc "Look up a required array attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectArrayE @@ var "cx" @@ var "v")

requireE :: TypedTermDefinition (InferenceContext -> String -> M.Map String JM.Value -> Result JM.Value)
requireE = define "requireE" $
  doc "Look up a required attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Optionals.cases (Maps.lookup (var "fname") (var "m" :: TypedTerm (M.Map String JM.Value))) (err @@ var "cx" @@ (Strings.cat $ list [string "required attribute ", Literals.showString (var "fname"), string " not found"])) (lambda "v" $ Phantoms.right (var "v"))

requireNumberE :: TypedTermDefinition (InferenceContext -> String -> M.Map String JM.Value -> Result Sci.Scientific)
requireNumberE = define "requireNumberE" $
  doc "Look up a required number attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectNumberE @@ var "cx" @@ var "v")

requireStringE :: TypedTermDefinition (InferenceContext -> String -> M.Map String JM.Value -> Result String)
requireStringE = define "requireStringE" $
  doc "Look up a required string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectStringE @@ var "cx" @@ var "v")

showJsonValue :: TypedTermDefinition (JM.Value -> String)
showJsonValue = define "showJsonValue" $
  doc "Convert a JSON value to its string representation" $
  lambda "v" $
    var "hydra.json.writer.printJson" @@ var "v"

stringToJsonValue :: TypedTermDefinition (String -> Either String JM.Value)
stringToJsonValue = define "stringToJsonValue" $
  doc "Parse a JSON string, returning Either for compatibility" $
  lambda "s" $
    cases Parsing._ParseResult (var "hydra.json.parser.parseJson" @@ var "s") Nothing [
      Parsing._ParseResult_success>>: lambda "success" $
        Phantoms.right (project Parsing._ParseSuccess Parsing._ParseSuccess_value @@ var "success"),
      Parsing._ParseResult_failure>>: lambda "failure" $
        Phantoms.left (project Parsing._ParseError Parsing._ParseError_message @@ var "failure")]


-- | String constants

unexpectedE :: TypedTermDefinition (InferenceContext -> String -> String -> Result a)
unexpectedE = define "unexpectedE" $
  doc "Construct an error for unexpected values" $
  lambda "cx" $ lambda "expected" $ lambda "found" $
    err @@ var "cx" @@ (Strings.cat $ list [string "Expected ", var "expected", string ", found: ", var "found"])


-- | JSON extraction helpers
