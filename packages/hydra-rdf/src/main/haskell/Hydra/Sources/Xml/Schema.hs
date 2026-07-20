module Hydra.Sources.Xml.Schema where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.xml.schema"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.\n" ++
      "See: https://www.w3.org/TR/xmlschema-2\n" ++
      "Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply\n" ++
      "      the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly\n" ++
      "      corresponding Hydra literal type."))}
  where
    definitions = [
      anySimpleType,
      anyType,
      anyURI,
      base64Binary,
      boolean_,
      byte_,
      constrainingFacet,
      datatype_,
      date,
      dateTime,
      decimal,
      double_,
      duration,
      entities,
      entity,
      float_,
      gDay,
      gMonth,
      gMonthDay,
      gYear,
      gYearMonth,
      hexBinary,
      id_,
      idref,
      idrefs,
      int_,
      integer_,
      language_,
      long_,
      nmtoken,
      notation,
      name_,
      negativeInteger,
      nonNegativeInteger,
      nonPositiveInteger,
      normalizedString,
      positiveInteger,
      qName,
      short_,
      string_,
      time,
      token,
      unsignedByte,
      unsignedInt,
      unsignedLong,
      unsignedShort]

anySimpleType :: TypeDefinition
anySimpleType = define "AnySimpleType" $
  doc "The XSD anySimpleType, the base type of all simple (non-list, non-union-composed) XML Schema datatypes" $
  T.wrap T.string

anyType :: TypeDefinition
anyType = define "AnyType" $
  doc "The XSD anyType, the root of the XML Schema type hierarchy" $
  T.wrap T.string

anyURI :: TypeDefinition
anyURI = define "AnyURI" $
  doc "The XSD anyURI datatype: a URI reference" $
  T.wrap T.string

base64Binary :: TypeDefinition
base64Binary = define "Base64Binary" $
  doc "The XSD base64Binary datatype: arbitrary binary data encoded as base64" $
  T.wrap T.string

boolean_ :: TypeDefinition
boolean_ = define "Boolean" $
  doc "The XSD boolean datatype: true or false" $
  T.wrap T.boolean

byte_ :: TypeDefinition
byte_ = define "Byte" $
  doc "The XSD byte datatype: an 8-bit signed integer" $
  T.wrap T.int8

constrainingFacet :: TypeDefinition
constrainingFacet = define "ConstrainingFacet" $
  doc "An XML Schema constraining facet, restricting the value space of a datatype" $
  see "https://www.w3.org/TR/xmlschema-2/#non-fundamental" $
  T.wrap T.unit -- TODO: concrete facets

datatype_ :: TypeDefinition
datatype_ = define "Datatype" $
  doc "The name of a built-in XML Schema datatype" $
  T.enum [
  "anySimpleType",
  "anyType",
  "anyURI",
  "base64Binary",
  "boolean",
  "byte",
  "date",
  "dateTime",
  "decimal",
  "double",
  "duration",
  "ENTITIES",
  "ENTITY",
  "float",
  "gDay",
  "gMonth",
  "gMonthDay",
  "gYear",
  "gYearMonth",
  "hexBinary",
  "ID",
  "IDREF",
  "IDREFS",
  "int",
  "integer",
  "language",
  "long",
  "NMTOKEN",
  "NOTATION",
  "name",
  "negativeInteger",
  "nonNegativeInteger",
  "nonPositiveInteger",
  "normalizedString",
  "positiveInteger",
  "qName",
  "short",
  "string",
  "time",
  "token",
  "unsignedByte",
  "unsignedInt",
  "unsignedLong",
  "unsignedShort"]

date :: TypeDefinition
date = define "Date" $
  doc "The XSD date datatype: a calendar date, optionally with a timezone" $
  T.wrap T.string

dateTime :: TypeDefinition
dateTime = define "DateTime" $
  doc "The XSD dateTime datatype: a date and time, optionally with a timezone" $
  T.wrap T.string

decimal :: TypeDefinition
decimal = define "Decimal" $
  doc "The XSD decimal datatype: an arbitrary-precision signed decimal number" $
  T.wrap T.string

double_ :: TypeDefinition
double_ = define "Double" $
  doc "The XSD double datatype: an IEEE 754 double-precision floating-point number" $
  T.wrap T.float64

duration :: TypeDefinition
duration = define "Duration" $
  doc "The XSD duration datatype: a duration of time" $
  T.wrap T.string

entities :: TypeDefinition
entities = define "ENTITIES" $
  doc "The XSD ENTITIES datatype: a whitespace-separated list of unparsed entity names" $
  T.wrap T.string

entity :: TypeDefinition
entity = define "ENTITY" $
  doc "The XSD ENTITY datatype: the name of an unparsed entity" $
  T.wrap T.string

float_ :: TypeDefinition
float_ = define "Float" $
  doc "The XSD float datatype: an IEEE 754 single-precision floating-point number" $
  T.wrap T.float32

gDay :: TypeDefinition
gDay = define "GDay" $
  doc "The XSD gDay datatype: a day of the month, recurring" $
  T.wrap T.string

gMonth :: TypeDefinition
gMonth = define "GMonth" $
  doc "The XSD gMonth datatype: a month of the year, recurring" $
  T.wrap T.string

gMonthDay :: TypeDefinition
gMonthDay = define "GMonthDay" $
  doc "The XSD gMonthDay datatype: a day of a specific month, recurring yearly" $
  T.wrap T.string

gYear :: TypeDefinition
gYear = define "GYear" $
  doc "The XSD gYear datatype: a calendar year" $
  T.wrap T.string

gYearMonth :: TypeDefinition
gYearMonth = define "GYearMonth" $
  doc "The XSD gYearMonth datatype: a specific month of a specific year" $
  T.wrap T.string

hexBinary :: TypeDefinition
hexBinary = define "HexBinary" $
  doc "The XSD hexBinary datatype: arbitrary binary data encoded as hexadecimal" $
  T.wrap T.string

id_ :: TypeDefinition
id_ = define "ID" $
  doc "The XSD ID datatype: an XML identifier attribute value, unique within a document" $
  T.wrap T.string

idref :: TypeDefinition
idref = define "IDREF" $
  doc "The XSD IDREF datatype: a reference to an XML ID attribute value" $
  T.wrap T.string

idrefs :: TypeDefinition
idrefs = define "IDREFS" $
  doc "The XSD IDREFS datatype: a whitespace-separated list of IDREF values" $
  T.wrap T.string

int_ :: TypeDefinition
int_ = define "Int" $
  doc "The XSD int datatype: a 32-bit signed integer" $
  T.wrap T.int32

integer_ :: TypeDefinition
integer_ = define "Integer" $
  doc "The XSD integer datatype: an arbitrary-precision signed integer" $
  T.wrap T.bigint

language_ :: TypeDefinition
language_ = define "Language" $
  doc "The XSD language datatype: an RFC 3066 language tag" $
  T.wrap T.string

long_ :: TypeDefinition
long_ = define "Long" $
  doc "The XSD long datatype: a 64-bit signed integer" $
  T.wrap T.int64

name_ :: TypeDefinition
name_ = define "Name" $
  doc "The XSD Name datatype: an XML Name (a token that may not start with a digit)" $
  T.wrap T.string

negativeInteger :: TypeDefinition
negativeInteger = define "NegativeInteger" $
  doc "The XSD negativeInteger datatype: an arbitrary-precision integer less than zero" $
  T.wrap T.bigint

nmtoken :: TypeDefinition
nmtoken = define "NMTOKEN" $
  doc "The XSD NMTOKEN datatype: an XML name token" $
  T.wrap T.string

nonNegativeInteger :: TypeDefinition
nonNegativeInteger = define "NonNegativeInteger" $
  doc "The XSD nonNegativeInteger datatype: an arbitrary-precision integer greater than or equal to zero" $
  T.wrap T.bigint

nonPositiveInteger :: TypeDefinition
nonPositiveInteger = define "NonPositiveInteger" $
  doc "The XSD nonPositiveInteger datatype: an arbitrary-precision integer less than or equal to zero" $
  T.wrap T.bigint

normalizedString :: TypeDefinition
normalizedString = define "NormalizedString" $
  doc "The XSD normalizedString datatype: a string with no carriage return, line feed, or tab characters" $
  T.wrap T.string

notation :: TypeDefinition
notation = define "NOTATION" $
  doc "The XSD NOTATION datatype: the name of a notation declared in the document's DTD" $
  T.wrap T.string

positiveInteger :: TypeDefinition
positiveInteger = define "PositiveInteger" $
  doc "The XSD positiveInteger datatype: an arbitrary-precision integer greater than zero" $
  T.wrap T.bigint

qName :: TypeDefinition
qName = define "QName" $
  doc "The XSD QName datatype: a namespace-qualified XML name" $
  T.wrap T.string

short_ :: TypeDefinition
short_ = define "Short" $
  doc "The XSD short datatype: a 16-bit signed integer" $
  T.wrap T.int16

string_ :: TypeDefinition
string_ = define "String" $
  doc "The XSD string datatype: a character string" $
  T.wrap T.string

time :: TypeDefinition
time = define "Time" $
  doc "The XSD time datatype: a time of day, optionally with a timezone" $
  T.wrap T.string

token :: TypeDefinition
token = define "Token" $
  doc "The XSD token datatype: a normalized string with no leading, trailing, or consecutive internal whitespace" $
  T.wrap T.string

unsignedByte :: TypeDefinition
unsignedByte = define "UnsignedByte" $
  doc "The XSD unsignedByte datatype: an 8-bit unsigned integer" $
  T.wrap T.uint8

unsignedInt :: TypeDefinition
unsignedInt = define "UnsignedInt" $
  doc "The XSD unsignedInt datatype: a 32-bit unsigned integer" $
  T.wrap T.uint32

unsignedLong :: TypeDefinition
unsignedLong = define "UnsignedLong" $
  doc "The XSD unsignedLong datatype: a 64-bit unsigned integer" $
  T.wrap T.uint64

unsignedShort :: TypeDefinition
unsignedShort = define "UnsignedShort" $
  doc "The XSD unsignedShort datatype: a 16-bit unsigned integer" $
  T.wrap T.uint16
