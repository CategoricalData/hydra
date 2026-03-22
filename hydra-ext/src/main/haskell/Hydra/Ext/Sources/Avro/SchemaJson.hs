module Hydra.Ext.Sources.Avro.SchemaJson where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
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
import qualified Hydra.Dsl.Module                     as Module
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
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
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
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Error                      as Error
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Avro
import qualified Hydra.Json.Model as JM
import qualified Hydra.Ext.Sources.Avro.Schema as AvroSchema
-- Result type alias (was previously imported from Staging module)
type Result a = Either (InContext Error) a


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

avroSchemaPhantomNs :: Namespace
avroSchemaPhantomNs = Namespace "hydra.ext.org.apache.avro.schema"

jsonModelNs :: Namespace
jsonModelNs = Namespace "hydra.json.model"

jsonParserNs :: Namespace
jsonParserNs = Namespace "hydra.json.parser"

jsonWriterNs :: Namespace
jsonWriterNs = Namespace "hydra.json.writer"

ns :: Namespace
ns = Namespace "hydra.ext.avro.schemaJson"

module_ :: Module
module_ = Module ns elements
    [jsonWriterNs, jsonParserNs]
    (AvroSchema.ns:jsonModelNs:KernelTypes.kernelTypesNamespaces) $
    Just "JSON serialization and deserialization for Avro schemas"
  where
    elements = [
      -- String constants
      toBinding avro_aliases,
      toBinding avro_array,
      toBinding avro_ascending,
      toBinding avro_boolean,
      toBinding avro_bytes,
      toBinding avro_default,
      toBinding avro_descending,
      toBinding avro_doc,
      toBinding avro_double,
      toBinding avro_enum,
      toBinding avro_fields,
      toBinding avro_fixed,
      toBinding avro_float,
      toBinding avro_ignore,
      toBinding avro_int,
      toBinding avro_items,
      toBinding avro_long,
      toBinding avro_map,
      toBinding avro_name,
      toBinding avro_namespace,
      toBinding avro_null,
      toBinding avro_order,
      toBinding avro_record,
      toBinding avro_size,
      toBinding avro_string,
      toBinding avro_symbols,
      toBinding avro_type,
      toBinding avro_values,
      -- Error helpers
      toBinding err,
      toBinding unexpectedE,
      -- JSON extraction helpers
      toBinding expectArrayE,
      toBinding expectNumberE,
      toBinding expectObjectE,
      toBinding expectStringE,
      toBinding requireE,
      toBinding requireArrayE,
      toBinding requireNumberE,
      toBinding requireStringE,
      toBinding optE,
      toBinding optArrayE,
      toBinding optStringE,
      toBinding showJsonValue,
      toBinding stringToJsonValue,
      -- Coder functions
      toBinding avroSchemaJsonCoder,
      toBinding avroSchemaStringCoder,
      -- Decode functions
      toBinding decodeAliases,
      toBinding decodeEnum,
      toBinding decodeField,
      toBinding decodeFixed,
      toBinding decodeNamedSchema,
      toBinding decodeOrder,
      toBinding decodeRecord,
      toBinding decodeSchema,
      toBinding getAnnotations]


-- | Error helpers

err :: TBinding (Context -> String -> Result a)
err = define "err" $
  doc "Construct an error result with a message in context" $
  lambda "cx" $ lambda "msg" $
    Ctx.failInContext (Error.errorOther $ Error.otherError (var "msg")) (var "cx")

unexpectedE :: TBinding (Context -> String -> String -> Result a)
unexpectedE = define "unexpectedE" $
  doc "Construct an error for unexpected values" $
  lambda "cx" $ lambda "expected" $ lambda "found" $
    err @@ var "cx" @@ (Strings.cat $ list [string "Expected ", var "expected", string ", found: ", var "found"])


-- | JSON extraction helpers

expectArrayE :: TBinding (Context -> JM.Value -> Result [JM.Value])
expectArrayE = define "expectArrayE" $
  doc "Extract a JSON array or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_array>>: lambda "v" $ Phantoms.right (var "v")]

expectNumberE :: TBinding (Context -> JM.Value -> Result Double)
expectNumberE = define "expectNumberE" $
  doc "Extract a JSON number or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_number>>: lambda "v" $ Phantoms.right (var "v")]

expectObjectE :: TBinding (Context -> JM.Value -> Result (M.Map String JM.Value))
expectObjectE = define "expectObjectE" $
  doc "Extract a JSON object or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_object>>: lambda "v" $ Phantoms.right (var "v")]

expectStringE :: TBinding (Context -> JM.Value -> Result String)
expectStringE = define "expectStringE" $
  doc "Extract a JSON string or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_string>>: lambda "v" $ Phantoms.right (var "v")]

requireE :: TBinding (Context -> String -> M.Map String JM.Value -> Result JM.Value)
requireE = define "requireE" $
  doc "Look up a required attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (err @@ var "cx" @@ (Strings.cat $ list [string "required attribute ", Literals.showString (var "fname"), string " not found"]))
      (lambda "v" $ Phantoms.right (var "v"))
      (Maps.lookup (var "fname") (var "m"))

requireArrayE :: TBinding (Context -> String -> M.Map String JM.Value -> Result [JM.Value])
requireArrayE = define "requireArrayE" $
  doc "Look up a required array attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectArrayE @@ var "cx" @@ var "v")

requireNumberE :: TBinding (Context -> String -> M.Map String JM.Value -> Result Double)
requireNumberE = define "requireNumberE" $
  doc "Look up a required number attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectNumberE @@ var "cx" @@ var "v")

requireStringE :: TBinding (Context -> String -> M.Map String JM.Value -> Result String)
requireStringE = define "requireStringE" $
  doc "Look up a required string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Eithers.bind (requireE @@ var "cx" @@ var "fname" @@ var "m")
      (lambda "v" $ expectStringE @@ var "cx" @@ var "v")

optE :: TBinding (String -> M.Map String JM.Value -> Maybe JM.Value)
optE = define "optE" $
  doc "Look up an optional attribute in a JSON object map" $
  lambda "k" $ lambda "m" $
    Maps.lookup (var "k") (var "m")

optArrayE :: TBinding (Context -> String -> M.Map String JM.Value -> Result (Maybe [JM.Value]))
optArrayE = define "optArrayE" $
  doc "Look up an optional array attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (Phantoms.right nothing)
      (lambda "v" $ Eithers.map (lambda "a" $ Maybes.pure (var "a")) (expectArrayE @@ var "cx" @@ var "v"))
      (Maps.lookup (var "fname") (var "m"))

optStringE :: TBinding (Context -> String -> M.Map String JM.Value -> Result (Maybe String))
optStringE = define "optStringE" $
  doc "Look up an optional string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (Phantoms.right nothing)
      (lambda "v" $ Eithers.map (lambda "s" $ Maybes.pure (var "s")) (expectStringE @@ var "cx" @@ var "v"))
      (Maps.lookup (var "fname") (var "m"))

showJsonValue :: TBinding (JM.Value -> String)
showJsonValue = define "showJsonValue" $
  doc "Convert a JSON value to its string representation" $
  lambda "v" $
    var "hydra.ext.avro.schemaJson.showJsonValue" @@ var "v"

stringToJsonValue :: TBinding (String -> Either String JM.Value)
stringToJsonValue = define "stringToJsonValue" $
  doc "Parse a JSON string, returning Either for compatibility" $
  lambda "s" $
    var "hydra.ext.avro.schemaJson.stringToJsonValue" @@ var "s"


-- | String constants

avro_aliases :: TBinding String
avro_aliases = define "avro_aliases" $ string "aliases"

avro_array :: TBinding String
avro_array = define "avro_array" $ string "array"

avro_ascending :: TBinding String
avro_ascending = define "avro_ascending" $ string "ascending"

avro_boolean :: TBinding String
avro_boolean = define "avro_boolean" $ string "boolean"

avro_bytes :: TBinding String
avro_bytes = define "avro_bytes" $ string "bytes"

avro_default :: TBinding String
avro_default = define "avro_default" $ string "default"

avro_descending :: TBinding String
avro_descending = define "avro_descending" $ string "descending"

avro_doc :: TBinding String
avro_doc = define "avro_doc" $ string "doc"

avro_double :: TBinding String
avro_double = define "avro_double" $ string "double"

avro_enum :: TBinding String
avro_enum = define "avro_enum" $ string "enum"

avro_fields :: TBinding String
avro_fields = define "avro_fields" $ string "fields"

avro_fixed :: TBinding String
avro_fixed = define "avro_fixed" $ string "fixed"

avro_float :: TBinding String
avro_float = define "avro_float" $ string "float"

avro_ignore :: TBinding String
avro_ignore = define "avro_ignore" $ string "ignore"

avro_int :: TBinding String
avro_int = define "avro_int" $ string "int"

avro_items :: TBinding String
avro_items = define "avro_items" $ string "items"

avro_long :: TBinding String
avro_long = define "avro_long" $ string "long"

avro_map :: TBinding String
avro_map = define "avro_map" $ string "map"

avro_name :: TBinding String
avro_name = define "avro_name" $ string "name"

avro_namespace :: TBinding String
avro_namespace = define "avro_namespace" $ string "namespace"

avro_null :: TBinding String
avro_null = define "avro_null" $ string "null"

avro_order :: TBinding String
avro_order = define "avro_order" $ string "order"

avro_record :: TBinding String
avro_record = define "avro_record" $ string "record"

avro_size :: TBinding String
avro_size = define "avro_size" $ string "size"

avro_string :: TBinding String
avro_string = define "avro_string" $ string "string"

avro_symbols :: TBinding String
avro_symbols = define "avro_symbols" $ string "symbols"

avro_type :: TBinding String
avro_type = define "avro_type" $ string "type"

avro_values :: TBinding String
avro_values = define "avro_values" $ string "values"


-- | Coder functions

avroSchemaJsonCoder :: TBinding (Context -> Coder Avro.Schema JM.Value)
avroSchemaJsonCoder = define "avroSchemaJsonCoder" $
  doc "Create a coder between Avro schemas and JSON values" $
  lambda "cx" $
    var "hydra.ext.avro.schemaJson.avroSchemaJsonCoder" @@ var "cx"

avroSchemaStringCoder :: TBinding (Context -> Coder Avro.Schema String)
avroSchemaStringCoder = define "avroSchemaStringCoder" $
  doc "Create a coder between Avro schemas and JSON strings" $
  lambda "cx" $
    var "hydra.ext.avro.schemaJson.avroSchemaStringCoder" @@ var "cx"


-- | Decode functions

decodeAliases :: TBinding (Context -> M.Map String JM.Value -> Result (Maybe [String]))
decodeAliases = define "decodeAliases" $
  doc "Decode aliases from a JSON object map" $
  lambda "cx" $ lambda "m" $
    var "hydra.ext.avro.schemaJson.decodeAliases" @@ var "cx" @@ var "m"

decodeEnum :: TBinding (Context -> M.Map String JM.Value -> Result Avro.NamedType)
decodeEnum = define "decodeEnum" $
  doc "Decode an Avro enum type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    var "hydra.ext.avro.schemaJson.decodeEnum" @@ var "cx" @@ var "m"

decodeField :: TBinding (Context -> M.Map String JM.Value -> Result Avro.Field)
decodeField = define "decodeField" $
  doc "Decode an Avro field from a JSON object map" $
  lambda "cx" $ lambda "m" $
    var "hydra.ext.avro.schemaJson.decodeField" @@ var "cx" @@ var "m"

decodeFixed :: TBinding (Context -> M.Map String JM.Value -> Result Avro.NamedType)
decodeFixed = define "decodeFixed" $
  doc "Decode an Avro fixed type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    var "hydra.ext.avro.schemaJson.decodeFixed" @@ var "cx" @@ var "m"

decodeNamedSchema :: TBinding (Context -> JM.Value -> Result Avro.Schema)
decodeNamedSchema = define "decodeNamedSchema" $
  doc "Decode a named Avro schema from a JSON value" $
  lambda "cx" $ lambda "value" $
    var "hydra.ext.avro.schemaJson.decodeNamedSchema" @@ var "cx" @@ var "value"

decodeOrder :: TBinding (Context -> String -> Result Avro.Order)
decodeOrder = define "decodeOrder" $
  doc "Decode an Avro field ordering from a string" $
  lambda "cx" $ lambda "o" $
    var "hydra.ext.avro.schemaJson.decodeOrder" @@ var "cx" @@ var "o"

decodeRecord :: TBinding (Context -> M.Map String JM.Value -> Result Avro.NamedType)
decodeRecord = define "decodeRecord" $
  doc "Decode an Avro record type from a JSON object map" $
  lambda "cx" $ lambda "m" $
    var "hydra.ext.avro.schemaJson.decodeRecord" @@ var "cx" @@ var "m"

decodeSchema :: TBinding (Context -> JM.Value -> Result Avro.Schema)
decodeSchema = define "decodeSchema" $
  doc "Decode an Avro schema from a JSON value" $
  lambda "cx" $ lambda "v" $
    var "hydra.ext.avro.schemaJson.decodeSchema" @@ var "cx" @@ var "v"

getAnnotations :: TBinding (M.Map String JM.Value -> M.Map String JM.Value)
getAnnotations = define "getAnnotations" $
  doc "Extract annotation entries (keys starting with @) from a JSON object map" $
  lambda "m" $
    Maps.fromList (Maybes.cat (Lists.map
      (lambda "entry" $ lets [
        "k">: Pairs.first (var "entry"),
        "v">: Pairs.second (var "entry")] $
        Logic.ifElse
          (Equality.equal (Strings.charAt (int32 0) (var "k")) (int32 64))  -- 64 = '@'
          (Maybes.pure (pair (Strings.fromList (Lists.drop (int32 1) (Strings.toList (var "k")))) (var "v")))
          nothing)
      (Maps.toList (var "m"))))
