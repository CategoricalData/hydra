module Hydra.Ext.Sources.Avro.Schema where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Json as Json


ns :: Namespace
ns = Namespace "hydra.ext.org.apache.avro.schema"

define :: String -> Type -> Binding
define = defineType ns

avro :: String -> Type
avro = typeref ns

json :: String -> Type
json = typeref $ Json.ns

module_ :: Module
module_ = Module ns elements [Json.ns] [Core.ns] $
    Just ("A model for Avro schemas. Based on the Avro 1.11.1 specification:\n" ++
      "  https://avro.apache.org/docs/1.11.1/specification")
  where
    elements = [
      array_,
      enum_,
      field_,
      fixed_,
      map_,
      named,
      namedType,
      order,
      primitive,
      record_,
      schema,
      union_]

array_ :: Binding
array_ = define "Array" $
  T.record [
    "items">: avro "Schema"]

enum_ :: Binding
enum_ = define "Enum" $
  T.record [
    "symbols">:
      doc ("a JSON array, listing symbols, as JSON strings. All symbols in an enum must be unique; " ++
        "duplicates are prohibited. Every symbol must match the regular expression [A-Za-z_][A-Za-z0-9_]* " ++
        "(the same requirement as for names)") $
      T.list T.string,
    "default">:
      doc ("A default value for this enumeration, used during resolution when the reader encounters " ++
        "a symbol from the writer that isn't defined in the reader's schema. " ++
        "The value provided here must be a JSON string that's a member of the symbols array") $
      T.maybe T.string]

field_ :: Binding
field_ = define "Field" $
  T.record [
    "name">:
      doc "a JSON string providing the name of the field"
      T.string,
    "doc">:
      doc "a JSON string describing this field for users" $
      T.maybe T.string,
    "type">:
      doc "a schema" $
      avro "Schema",
    "default">:
      doc "default value for this field, only used when reading instances that lack the field for schema evolution purposes" $
      T.maybe $ json "Value",
    "order">:
      doc "specifies how this field impacts sort ordering of this record" $
      T.maybe $ avro "Order",
    "aliases">:
      doc "a JSON array of strings, providing alternate names for this field" $
      T.maybe $ T.list T.string,
    "annotations">:
      doc "Any additional key/value pairs attached to the field" $
      T.map T.string $ json "Value"]

fixed_ :: Binding
fixed_ = define "Fixed" $
  T.record [
    "size">:
      doc "an integer, specifying the number of bytes per value"
      T.int32]

map_ :: Binding
map_ = define "Map" $
  T.record [
    "values">: avro "Schema"]

named :: Binding
named = define "Named" $
  T.record [
    "name">:
      doc "a string naming this schema"
      T.string,
    "namespace">:
      doc "a string that qualifies the name" $
      T.maybe T.string,
    "aliases">:
      doc "a JSON array of strings, providing alternate names for this schema" $
      T.maybe $ T.list T.string,
    "doc">:
      doc "a JSON string providing documentation to the user of this schema" $
      T.maybe T.string,
    "type">: avro "NamedType",
    "annotations">:
      doc "Any additional key/value pairs attached to the type" $
      T.map T.string $ json "Value"]

namedType :: Binding
namedType = define "NamedType" $
  T.union [
    "enum">: avro "Enum",
    "fixed">: avro "Fixed",
    "record">: avro "Record"]

order :: Binding
order = define "Order" $
  T.enum ["ascending", "descending", "ignore"]

primitive :: Binding
primitive = define "Primitive" $
  T.union [
    "null">:
      doc "no value" T.unit,
    "boolean">:
      doc "A binary value" T.unit,
    "int">:
      doc "32-bit signed integer" T.unit,
    "long">:
      doc "64-bit signed integer" T.unit,
    "float">:
      doc "single precision (32-bit) IEEE 754 floating-point number" T.unit,
    "double">:
      doc "double precision (64-bit) IEEE 754 floating-point number" T.unit,
    "bytes">:
      doc "sequence of 8-bit unsigned bytes" T.unit,
    "string">:
      doc "unicode character sequence" T.unit]

record_ :: Binding
record_ = define "Record" $
  T.record [
    "fields">:
      doc "a JSON array, listing fields" $
      T.list $ avro "Field"]

schema :: Binding
schema = define "Schema" $
  T.union [
    "array">: avro "Array",
    "map">: avro "Map",
    "named">: avro "Named",
    "primitive">: avro "Primitive",
    "reference">: -- Note: "reference" is not described in the Avro specification; this has been added
      doc "A reference by name to a previously defined type" T.string,
    "union">: avro "Union"
  ]

union_ :: Binding
union_ = define "Union" $
  T.wrap $ T.list $ avro "Schema"
